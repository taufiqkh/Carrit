(ns carrit.chunk-world
  "Manages chunks in a world map."
  (:use carrit.region-file)
  (:gen-class
    :constructors {[String] []}
    :init init
    :methods [[hasChunk [int int int] boolean]
              [getChunk[int int int] void]]
    ))
; Java interoperable class for exposing chunk API

(def MINECRAFT_DIR "Whole New World")

(defn -init [world-name]
  (if-let [save-dir (load-save-dir world-name)]
    (let [origin-descriptor (create-file-descriptor 0 0 0)]
      (read-region-file origin-descriptor ((save-dir :region-map) (origin-descriptor :filename)))))
  nil)

(defn -main [& options]
  (-init MINECRAFT_DIR))

(defn -hasChunk [x y z] false)

(defn -getChunk [x y z] nil)