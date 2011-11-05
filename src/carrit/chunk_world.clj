(ns carrit.chunk-world
  (:use carrit.region-file)
  (:gen-class
    :constructors {[String] []}
    :init init
    :methods [[hasChunk [] boolean]
              [getChunk[int int int] void]]
    ))

(def MINECRAFT_DIR "Whole New World")

(defn -init [world-name]
  (if-let [save-dir (load-save-dir world-name)]
    (let [origin-descriptor (create-file-descriptor 0 0 0)]
      (read-region-file origin-descriptor ((save-dir :region-map) (origin-descriptor :filename)))))
  nil)

(defn -main [& options]
  (-init MINECRAFT_DIR))

(defn -hasChunk [] false)

(defn -getChunk [x y z] nil)