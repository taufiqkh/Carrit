(ns carrit.region-file
  (:require [clojure.contrib.logging :as logging]
            [clojure.contrib.duck-streams :as duck-streams]
            [clojure.contrib.generic.math-functions :as math]
            [clojure.contrib.string :as string])
  (:import java.io.File
           java.util.Arrays
           java.util.zip.Inflater)
  (:use carrit.named-binary-tag
        carrit.byte-convert))
(set! *warn-on-reflection* true)
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def EXPECTED_SAVE_ENTRIES #{"data" "region" "level.dat"})

(defn info
  "Logs the specified string using a standard logger"
  [log-message & parameters]
  (logging/info (apply format log-message parameters)))

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

(defn map-region-dir [files]
  "Given a save game directory finds the region directory and extracts the
files for that directory, mapped by file name."
  (if (empty? files)
    (info "Files exhausted before region directory found.")
    (let [^File first-file (first files)]
      (if (= "region" (.getName first-file))
        (if (.isDirectory first-file)
          (let [region-file-seq (file-seq first-file)
                region-file-names (map #(.getName ^File %) region-file-seq)]
            (zipmap region-file-names region-file-seq))
          nil)
        (recur (next files))))))

(def ^{:doc "Bitshift for x chunk to determine file name"} CHUNK_X_SHIFT 5)
(def
  ^{:doc "Number of chunks per region along the x axis. Offset multiplier for x location for chunk location headers"}
   CHUNK_X_MULTIPLIER (int (math/pow 2 CHUNK_X_SHIFT)))
(def ^{:doc "Bitshift for z chunk to determine file name"} CHUNK_Z_SHIFT 5)
(def
  ^{:doc "Number of chunks per region along the z axis. Offset multiplier for z location for chunk location headers"}
   CHUNK_Z_MULTIPLIER (int (math/pow 2 CHUNK_Z_SHIFT)))
(def ^{:doc "Size of chunk location offset, in bytes"} CHUNK_LOCATION_OFFSET_SIZE 3)
(def ^{:doc "Size of chunk location sector, in bytes"} CHUNK_LOCATION_SECTOR_SIZE 1)
(def ^{:doc "Size of chunk location data, in bytes" }
      CHUNK_LOCATION_SIZE
  (+ CHUNK_LOCATION_OFFSET_SIZE CHUNK_LOCATION_SECTOR_SIZE))
(def ^{:doc "Size of a chunk sector in bytes"} CHUNK_SECTOR_SIZE 4096)
(def ^{:doc "Size of chunk timestamp data, in bytes"} CHUNK_TIMESTAMP_SIZE 4)
(def ^{:doc "Size of the length header for chunk data, in bytes"} CHUNK_DATA_LENGTH_HEADER_SIZE 4)
(def ^{:doc "Size of the compression type header, in bytes"} CHUNK_COMPRESSION_TYPE_SIZE 1)
(def ^{:doc "Number of chunks in a file"} CHUNKS_PER_REGION (* CHUNK_X_MULTIPLIER CHUNK_Z_MULTIPLIER))

(defn calc-chunk-header-offset [header-size x z]
  "Given the x, y, z coordinates, calculates the offset, in bytes, of the
chunk header in a region file"
  (* header-size (+ (mod x CHUNK_X_MULTIPLIER) (* (mod z CHUNK_Z_MULTIPLIER) CHUNK_X_MULTIPLIER))))

; TODO: probably not needed, call underlying function directly
(defn chunk-timestamp [^bytes timestamp-byte-array]
  "Returns a chunk location from the specified timestamp byte array. The array
must be at least the size of a timestamp."
  (num-from-byte-array timestamp-byte-array 0 CHUNK_TIMESTAMP_SIZE))

(defn chunk-length-byte-array [^bytes chunk-length-byte-array]
  "Returns the chunk length from the specified chunk header array. The array
must be at least the size of a chunk length header."
  (num-from-byte-array chunk-length-byte-array 0 CHUNK_DATA_LENGTH_HEADER_SIZE))

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

(defn expand-arrays [arrays ^Integer length]
  "For a given sequence of arrays, returns a single array of the specified
length, containing the contents of the arrays up to that length."
  {:pre [(> length 0) (<= length (reduce + 0 (map #(alength ^bytes %) arrays)))]}
  (let [^bytes first-array (first arrays)
        ^bytes destination (Arrays/copyOf first-array length)]
    (loop [arrays-rem (rest arrays) offset (alength first-array) length-remaining (- length offset)]
      (if (empty? arrays-rem)
        destination
        (let [^bytes next-array (first arrays-rem) next-length (min (alength next-array) length-remaining)]
          (System/arraycopy next-array 0 destination offset next-length)
          (recur (rest arrays-rem) (+ offset (alength next-array)) (- length-remaining next-length)))))))

; TODO: Fix infinite loop in case inflator doesn't think it's finished once
; it's reached compressed length
(defn inflate-chunk-data [chunk-byte-array offset compressed-length alloc-length]
  "Reads chunk data from the specified array, using the given offset and length"
  (info "Inflating chunk data offset %d compressed-length %d alloc-length %d"
        offset compressed-length alloc-length)
  (if (<= compressed-length 0)
    {:length 0 :data nil}
    (let [inflater (Inflater.)]
      (.setInput inflater chunk-byte-array offset compressed-length)
      (loop [arrays []]
        (let [next-byte-array (byte-array CHUNK_SECTOR_SIZE)]
          (.inflate inflater next-byte-array 0 CHUNK_SECTOR_SIZE)
          (info "Inflator %d: Read: %d Written %d Finished %b" offset (.getBytesRead inflater) (.getBytesWritten inflater) (.finished inflater))
          (if (.finished inflater)
            (let [bytes-written (.getBytesWritten inflater)]
              {:length bytes-written :data (expand-arrays arrays bytes-written)})
            (recur (conj arrays next-byte-array))))))))

(defn read-chunk-data [chunk-byte-array offset alloc-length]
  {:pre [(>= offset 0) (> alloc-length 0)]}
  "Reads the chunk header and data at a specified offset. Contains side effects."
  (let [compressed-length (num-from-byte-array chunk-byte-array offset CHUNK_DATA_LENGTH_HEADER_SIZE)
        compression-type (num-from-byte-array chunk-byte-array offset CHUNK_COMPRESSION_TYPE_SIZE)
        data-offset (+ offset CHUNK_DATA_LENGTH_HEADER_SIZE CHUNK_COMPRESSION_TYPE_SIZE)]
    {:compressed-length compressed-length
     :compression-type compression-type
     :data (inflate-chunk-data chunk-byte-array data-offset (dec compressed-length) alloc-length)}))

(defn read-chunk [chunk-byte-array location-offset timestamp-offset]
  "Reads chunk data from the specified offsets"
  (let [data-location (* (num-from-byte-array chunk-byte-array location-offset CHUNK_LOCATION_OFFSET_SIZE)
                         CHUNK_SECTOR_SIZE)
        sectors (num-from-byte-array chunk-byte-array
                                           (+ location-offset CHUNK_LOCATION_OFFSET_SIZE)
                                           CHUNK_LOCATION_SECTOR_SIZE)
        chunk-map {:data-offset data-location
                   :sectors sectors
                   :timestamp (num-from-byte-array chunk-byte-array timestamp-offset CHUNK_TIMESTAMP_SIZE)}]
    (if (zero? data-location)
      chunk-map
      (let []
        (info "Offset: %d Sectors: %d Timestamp: %d" data-location sectors (chunk-map :timestamp))
        (merge chunk-map (read-chunk-data chunk-byte-array data-location (* sectors CHUNK_SECTOR_SIZE)))))))

(defn read-region-file [file-descriptor file]
  "Reads a region file. Contains side-effects"
  (let [chunk-byte-array (duck-streams/to-byte-array file)
        loc-keys (region-loc-keys (:xRegion file-descriptor) (:zRegion file-descriptor))
        timestamp-header-offset (* CHUNKS_PER_REGION CHUNK_LOCATION_SIZE)]
      (loop [region-map {}
           location-read-from 0
           timestamp-read-from (* CHUNKS_PER_REGION CHUNK_LOCATION_SIZE)
           loc-keys-rem loc-keys]
        (if (empty? loc-keys-rem)
          region-map
          (let [[x z] (peek loc-keys-rem)]
            (recur (assoc region-map [x z]
                          (read-chunk chunk-byte-array location-read-from timestamp-read-from))
                   (calc-chunk-header-offset CHUNK_LOCATION_SIZE x z)
                   (+ timestamp-header-offset (calc-chunk-header-offset CHUNK_TIMESTAMP_SIZE x z))
                   (pop loc-keys-rem)))))))

(defn create-file-descriptor [x y z]
  "Generates a region file name for the specified coordinates."
  ; Region determined by right bit shifting x and z
  (let [shift-map {:x CHUNK_X_SHIFT, :z CHUNK_Z_SHIFT}
        val-map {:x x, :z z}
        [xRegion zRegion] (map #(bit-shift-right (val-map %) (shift-map %)) [:x :z])]
    {:filename (format "r.%d.%d.mcr" xRegion zRegion), :xRegion xRegion, :zRegion zRegion}))

(defn load-save-dir [^String dirname]
  (if-let [save-dir-files (verify-save-dir (File. dirname))]
    (if-let [region-dir-files (map-region-dir save-dir-files)]
      {:save-dir save-dir-files
       :region-map region-dir-files}
      (info "Couldn't map region directory files for %s." dirname))
    nil))