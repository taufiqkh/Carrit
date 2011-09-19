(ns carrit
  (:require [clojure.contrib.logging :as logging]
            [clojure.contrib.duck-streams :as duck-streams]
            [clojure.contrib.generic.math-functions :as math]
            [clojure.contrib.string :as string])
  (:import [java.io File])
  (:gen-class))
(set! *warn-on-reflection* true)
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def EXPECTED_SAVE_ENTRIES #{"data" "region" "level.dat"})

; Minecraft save directory, hard-coded to a test directory until I get around
; to adding options
(def MINECRAFT_DIR "Whole New World")

(defn base-n
  "Converts a number to the specified base, using a digit sequence. The digit
sequence must match the base"
  ([base digits number]
    (let [digit-seq (if (neg? number)
                      (cons "-" (base-n base digits (- number) '()))
                      (base-n base digits number '()))]
      (reduce str digit-seq)))
  ([base digits number acc]
    "Converts a number to the specified base, using a digit sequence and
accumulator. The accumulator is a list containing the digits (from the right)
so far."
    (if (= number 0)
      (if (empty? acc) '("0") acc)
      (let [remainder (rem number base)]
        (recur base
               digits
               (/ (- number remainder) base)
               (cons (string/get digits remainder) acc))))))

; Hackety hack
(defn base-36 [number]
  "Converts a number to a string in base 36."
  (base-n number "0123456789abcdefjhijklmnopqrstuvwxyz"))

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

(defn chunk-num-from-byte-array [^bytes chunk-byte-array offset length]
  "Returns a number from a big-endian chunk byte array using entries up to the
specified length. The length of the array must be at least the specified
length."
  {:pre [(>= (alength chunk-byte-array) length)]}
  (reduce + (for [i (range 0 length)]
              (bit-shift-left #^Integer (aget chunk-byte-array i) (- length i)))))

; TODO: probably not needed, call underlying function directly
(defn chunk-location [^bytes location-byte-array]
  "Returns a chunk location from the specified location byte array. The array
must be at least the size of a location."
  (chunk-num-from-byte-array location-byte-array 0 CHUNK_LOCATION_SIZE))

; TODO: probably not needed, call underlying function directly
(defn chunk-timestamp [^bytes timestamp-byte-array]
  "Returns a chunk location from the specified timestamp byte array. The array
must be at least the size of a timestamp."
  (chunk-num-from-byte-array timestamp-byte-array 0 CHUNK_TIMESTAMP_SIZE))

(defn chunk-length-byte-array [^bytes chunk-length-byte-array]
  "Returns the chunk length from teh specified chunk header array. The array
must be at least the size of a chunk length header."
  (chunk-num-from-byte-array chunk-length-byte-array 0 CHUNK_DATA_LENGTH_HEADER_SIZE))

(defn read-chunk-file [file-descriptor]
  (let [chunk-byte-array (duck-streams/to-byte-array (File. (:file-name file-descriptor)))]
    ; chunk location:
    (chunk-num-from-byte-array chunk-byte-array 0 CHUNK_LOCATION_SIZE))
  nil)

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