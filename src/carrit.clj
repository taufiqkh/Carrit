(ns carrit
  (:require [clojure.contrib.logging :as logging]
            [clojure.java.io :as io])
  (:gen-class))
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def expected-save-entries #{"data" "region" "level.dat"})

; Minecraft save directory, hard-coded to a test directory until I get around
; to adding options
(def minecraft-dir "Whole New World")

(defn info
  "Logs the specified string using a standard logger"
  [log-message parameters]
  (logging/info (format log-message parameters)))

(defn verify-directory [directory]
  (info "Verifying directory %s" (.getPath directory))
  (if (.isDirectory directory)
    (if (clojure.set/subset? expected-save-entries (into #{} (.list directory)))
      directory
      nil)
    nil))

(defn -main [& options]
  (verify-directory (io/file minecraft-dir)))