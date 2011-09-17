(ns carrit
  (:require [clojure.contrib.logging :as logging]
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

(def ^{:doc "Size of a chunk, in blocks"} CHUNK_SIZE 32)
(def ^{:doc "Size of chunk location data, in bytes" } CHUNK_LOCATION_SIZE 4)
(def ^{:doc "Size of chunk timestamp data, in bytes"} CHUNK_TIMESTAMP_SIZE 4)
(def ^{:doc "Size of the length header for chunk data, in bytes"} CHUNK_DATA_LENGTH_HEADER_SIZE 4)

(defn calc-chunk-location-offset [x z]
  "Given the x, y, z coordinates, calculates the offset, in bytes, of the
chunk location in a region file"
  (* CHUNK_LOCATION_SIZE (+ (mod x CHUNK_SIZE) (* (mod z CHUNK_SIZE) CHUNK_SIZE))))
(defn chunk-location [location-byte-array]
  nil)

(defn read-chunk-file [file-name]
  (slurp file-name))

(defn create-file-name [x y z]
  "Generates a region file name for the specified coordinates."
  (let [[xChunk yChunk zChunk]
        (map #(with-precision 1 :rounding FLOOR (/ %1 CHUNK_SIZE)) [x y z])]
    format "r.%d.%d.mcr" [xChunk zChunk]))

(defn -main [& options]
  (if-let [save-dir-files (verify-save-dir (File. MINECRAFT_DIR))]
    (if-let [region-dir-files (extract-region-dir save-dir-files)]
      nil
      nil)
    nil))