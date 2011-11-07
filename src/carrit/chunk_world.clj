(ns carrit.chunk-world
  "Manages chunks in a world map."
  (:use carrit.region-file)
  (:import com.quiptiq.carrit.ChunkWorld)
  (:gen-class
    :constructors {[] []}
    :init init
    :methods [[loadChunkWorld [String] com.quiptiq.carrit.ChunkWorld]]
    ))
; Java interoperable class for exposing chunk API

(def MINECRAFT_DIR "Whole New World")

(defn -init [] nil)

(defn -hasChunk [x y z] false)

(defn -getChunk [x y z] nil)

(defn -asChunkWorld []
  "Returns a Java ChunkWorld object"
  nil)

(defn -loadChunkWorld [world-name]
  (let [loaded-world
        (if-let [save-dir (load-save-dir world-name)]
          (let [origin-descriptor (create-file-descriptor 0 0 0)]
            (read-region-file origin-descriptor ((save-dir :region-map) (origin-descriptor :filename))))
        nil)]
    (reify ChunkWorld
      (hasChunk [this x z]
                false)
      (getChunk [this x z] nil)
      (getChunkSizeX [_] 0)
      (getChunkSizeY [_] 0)
      (getChunkSizeZ [_] 0))))

(defn -main [& options]
  (-loadChunkWorld MINECRAFT_DIR))