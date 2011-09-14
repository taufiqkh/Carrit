(ns carrit
  (:require [clojure.contrib.logging :as logging]
            [clojure.java.io :as io])
  (:gen-class))
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def EXPECTED_SAVE_ENTRIES #{"data" "region" "level.dat"})

; Minecraft save directory, hard-coded to a test directory until I get around
; to adding options
(def MINECRAFT_DIR "Whole New World")

(def COORDINATE_BIT_SIZE 32)

(defn info
  "Logs the specified string using a standard logger"
  [log-message parameters]
  (logging/info (format log-message parameters)))

(defn verify-save-dir [directory]
  "Verifies that a given directory contains the files/directories expected in a
save game directory and returns a sequence of those files."
  (info "Verifying directory %s" (.getPath directory))
  (if (.isDirectory directory)
    (let [files (file-seq directory) filenames (set (map #(.getName %) files))]
      (if (clojure.set/subset? EXPECTED_SAVE_ENTRIES filenames)
        files
        nil))
    nil))

(defn extract-region-dir [files]
  (if (empty? files)
    nil
    (let [first-file (first files)]
      (if (= "region" first-file)
        (if (.isDirectory first-file)
          first-file
          nil)
        (recur (next files))))))

(defn read-chunk-file [file-name]
  (slurp file-name))

(defn create-file-name [x y z]
  "Generates a region file name for the specified coordinates."
  (let [[xChunk yChunk zChunk]
        (map #(with-precision 1 :rounding FLOOR (/ %1 COORDINATE_BIT_SIZE)) [x y z])]
    format "r.%d.%d.mcr" [xChunk zChunk]))

(defn -main [& options]
  (if-let [save-dir-files (verify-save-dir (io/file MINECRAFT_DIR))]
    (if-let [region-dir-files (extract-region-dir save-dir-files)]
      nil
      nil)
    nil))