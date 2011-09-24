(ns carrit.core
  (:require [clojure.contrib.logging :as logging]
            [clojure.contrib.duck-streams :as duck-streams]
            [clojure.contrib.generic.math-functions :as math]
            [clojure.contrib.string :as string])
  (:import java.io.File
           java.util.Arrays
           java.util.zip.Inflater)
  (:gen-class))
(set! *warn-on-reflection* true)
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def EXPECTED_SAVE_ENTRIES #{"data" "region" "level.dat"})

; Minecraft save directory, hard-coded to a test directory until I get around
; to adding options
(def MINECRAFT_DIR "Whole New World")

(defn info
  "Logs the specified string using a standard logger"
  [log-message parameters]
  (logging/info (format log-message parameters)))

(defn verify-save-dir [^File directory]
  "Verifies that a given directory contains the files/directories expected in a
save game directory and returns a sequence of those files."
  (info "Verifying directory %s" (.getPath directory))
  (if (.isDirectory directory)
    (let [files (file-seq directory) filenames (set (map (fn [^File file] (.getName file)) files))]
      (if (clojure.set/subset? EXPECTED_SAVE_ENTRIES filenames)
        files
        nil))
    nil))

(defn extract-region-dir [files]
  "Given a save game directory finds the region directory and extracts the
files for that directory."
  (if (empty? files)
    nil
    (let [^File first-file (first files)]
      (if (= "region" first-file)
        (if (.isDirectory first-file)
          first-file
          nil)
        (recur (next files))))))

(def ^{:doc "Bitshift for x chunk to determine file name"} CHUNK_X_SHIFT 5)
(def
  ^{:doc "Number of chunks per region along the x axis. Offset multiplier for x location for chunk location headers"}
   CHUNK_X_MULTIPLIER (math/pow 2 CHUNK_X_SHIFT))
(def ^{:doc "Bitshift for z chunk to determine file name"} CHUNK_Z_SHIFT 5)
(def
  ^{:doc "Number of chunks per region along the z axis. Offset multiplier for z location for chunk location headers"}
   CHUNK_Z_MULTIPLIER (math/pow 2 CHUNK_Z_SHIFT))
(def ^{:doc "Size of chunk location data, in bytes" } CHUNK_LOCATION_SIZE 4)
(def ^{:doc "Size of chunk timestamp data, in bytes"} CHUNK_TIMESTAMP_SIZE 4)
(def ^{:doc "Size of the length header for chunk data, in bytes"} CHUNK_DATA_LENGTH_HEADER_SIZE 4)
(def ^{:doc "Size of the compression type header, in bytes"} CHUNK_COMPRESSION_TYPE_SIZE 1)
(def ^{:doc "Number of chunks in a file"} CHUNKS_PER_REGION (* CHUNK_X_MULTIPLIER CHUNK_Z_MULTIPLIER))

(defn calc-chunk-header-offset [header-offset x z]
  "Given the x, y, z coordinates, calculates the offset, in bytes, of the
chunk header in a region file"
  (* header-offset (+ (mod x CHUNK_X_MULTIPLIER) (* (mod z CHUNK_Z_MULTIPLIER) CHUNK_X_MULTIPLIER))))

(defn chunk-num-from-byte-array [^bytes chunk-byte-array start-index length]
  "Returns a number from a big-endian chunk byte array using entries up to the
specified length. The length of the array must be at least the specified
length."
  {:pre [(>= (alength chunk-byte-array) (dec (+ start-index length)))]}
  (let [end-index (dec (+ start-index length))] 
    (reduce + (for [i (range start-index end-index)]
                (bit-shift-left #^Integer (aget chunk-byte-array i) (- end-index i))))))

; TODO: probably not needed, call underlying function directly
(defn chunk-timestamp [^bytes timestamp-byte-array]
  "Returns a chunk location from the specified timestamp byte array. The array
must be at least the size of a timestamp."
  (chunk-num-from-byte-array timestamp-byte-array 0 CHUNK_TIMESTAMP_SIZE))

(defn chunk-length-byte-array [^bytes chunk-length-byte-array]
  "Returns the chunk length from the specified chunk header array. The array
must be at least the size of a chunk length header."
  (chunk-num-from-byte-array chunk-length-byte-array 0 CHUNK_DATA_LENGTH_HEADER_SIZE))

(defn chunk-range [region shift]
  "Derives a chunk coordinate range from a region"
  (range (bit-shift-left region shift) (bit-shift-left (inc region) shift)))

(defn region-loc-keys [x-region z-region]
  "Creates a set of location keys of [x, z] for all x, z in a region"
  (let [x-range (chunk-range x-region CHUNK_X_SHIFT)
        z-range (chunk-range z-region CHUNK_Z_SHIFT)]
    ;loc-key (reduce conj (map (fn [x inner-z] (map (fn [z] (vector x z)) inner-z)) x-range (repeat z-range)))]
    ; Need to rewrite for clarity:
    (loop [x-rem x-range
           loc-key-acc []]
      (if (empty? x-rem)
        loc-key-acc
        (recur (rest x-rem)
          (loop [x (first x-rem)
                 z-rem z-range
                 inner-loc-key-acc loc-key-acc]
            (if (empty? z-rem)
              inner-loc-key-acc
              (recur x (next z-rem) (conj inner-loc-key-acc [(first x-rem) (first z-rem)])))))))))

(defn read-chunk-file [file-descriptor]
  "Reads a chunk file. Contains side-effects"
  (let [chunk-byte-array (duck-streams/to-byte-array (File. (:file-name file-descriptor)))
        loc-keys (region-loc-keys (:xRegion file-descriptor) (:zRegion file-descriptor))]
      (loop [location-map {}
           read-from 0
           loc-keys-rem loc-keys]
        (if (empty? loc-keys-rem)
          location-map
          (recur (assoc location-map
                        (peek loc-keys-rem)
                        (chunk-num-from-byte-array chunk-byte-array read-from CHUNK_LOCATION_SIZE))
                 (+ read-from CHUNK_LOCATION_SIZE)
                 (pop loc-keys-rem))))))

(defn expand-arrays [arrays ^Integer length]
  "For a given sequence of arrays, returns a single array of the specified
length, containing the contents of the arrays up to that length."
  {:pre (and (> length 0) (<= length (reduce + 0 (map #(alength ^bytes %) arrays))))}
  (let [^bytes first-array (first arrays)
        ^bytes destination (Arrays/copyOf first-array length)]
    (loop [arrays-rem (rest arrays) offset (alength first-array) length-remaining (- length offset)]
      (if (empty? arrays-rem)
        destination
        (let [^bytes next-array (first arrays-rem) next-length (min (alength next-array) length-remaining)]
          (System/arraycopy next-array 0 destination offset next-length)
          (recur (rest arrays-rem) (+ offset (alength next-array)) (- length-remaining next-length)))))))

(def ALLOCATION-BLOCK 4096)
(defn read-chunk-data [chunk-byte-array offset length]
  "Reads chunk data from the specified array, using the given offset and length"
  (let [inflater (Inflater.)]
    (.setInput inflater chunk-byte-array offset length)
    (loop [arrays []]
      (let [next-byte-array (byte-array ALLOCATION-BLOCK)]
        (.inflate inflater next-byte-array offset ALLOCATION-BLOCK)
        (if (.finished inflater)
          (let [bytes-written (.getBytesWritten inflater)]
            {:length bytes-written :data (expand-arrays arrays bytes-written)})
          (recur (conj arrays next-byte-array)))))))

(defn create-file-descriptor [x y z]
  "Generates a region file name for the specified coordinates."
  ; Region determined by right bit shifting x and z
  (let [shift-map {x CHUNK_X_SHIFT, z CHUNK_Z_SHIFT} [xRegion zRegion]
        (map #(bit-shift-right % (shift-map %)) [x z])]
    {:filename (format "r.%d.%d.mcr" xRegion zRegion), :xRegion xRegion, :zRegion zRegion}))

(defn -main [& options]
  (if-let [save-dir-files (verify-save-dir (File. MINECRAFT_DIR))]
    (if-let [region-dir-files (extract-region-dir save-dir-files)]
      nil
      nil)
    nil))