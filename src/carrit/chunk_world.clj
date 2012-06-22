(ns carrit.chunk-world
  "Manages chunks in a world map."
  (:use carrit.region-file
        carrit.byte-convert
        carrit.named-binary-tag)
  (:import com.quiptiq.carrit.ChunkWorld
           com.quiptiq.carrit.Chunk
           com.quiptiq.carrit.ChunkBlock)
  (:gen-class
    :constructors {[] []}
    :init init
    :methods [[loadChunkWorld [String] com.quiptiq.carrit.ChunkWorld]]
    ))
; Java interoperable class for exposing chunk API

(def minecraft-dir "Whole New World")

(def ^{:doc "Number of blocks along the x axis of a chunk"} chunk-size-x 16)
(def ^{:doc "Number of blocks along the y axis of a chunk"} chunk-size-y 16)
(def ^{:doc "Number of blocks along the z axis of a chunk"} chunk-size-z 16)

(def ^{:doc "Name of tag containing blocks"} tag-blocks "Blocks")

(defn -init [] nil)

(defrecord World [chunk-world data name])

(def ^{:doc "Default sea level for dummy terrain generation"} default-sea-level 63)

(defn get-chunk-map [world-data x z]
  "Given world data, returns the chunk map containing the specified coordinates"
  ([x z] ([x z] world-data)))

(defn gen-dummy-block-ids []
  (concat (repeat default-sea-level (byte 1)) (repeat (- chunk-size-y default-sea-level) (byte 0))))

(defn gen-dummy-data [column]
  "Generate dummy blocks for the chunk, distributed according to the following:
Blocks[ y + z * ChunkSizeY(=128) + x * ChunkSizeY(=128) * ChunkSizeZ(=16) ]"
  (byte-array (reduce concat [] (repeat (* chunk-size-x chunk-size-z) column))))

(defn dummy-chunk-block [chunk-data x y z blockTypeId skyLight]
  (reify ChunkBlock
    (getBlockTypeId [_] blockTypeId)
    (getBlockData [_] (byte 0))
    (getLightEmitted [_] (byte 0))
    (getSkyLight [_] skyLight)
    (getX [_] x)
    (getY [_] y)
    (getZ [_] z)))

(defn get-chunk-block [chunk-data x y z blockTypeId skyLight]
  (reify ChunkBlock
    (getBlockTypeId [_] blockTypeId)
    (getBlockData [_] (byte 0))
    (getLightEmitted [_] (byte 0))
    (getSkyLight [_] skyLight)
    (getX [_] x)
    (getY [_] y)
    (getZ [_] z)))

(defn dummy-chunk [chunk-world x y z]
  (reify Chunk
    ; Ids of the blocks in this chunk.
    (getBlockIds [_] ;(byte-array (gen-dummy-block-ids)));
                 (gen-dummy-data (gen-dummy-block-ids)))
    ; Ancillary data for the blocks in this chunk, 4 bits per block.
    (getBlockData [_] (gen-dummy-data (repeat (/ chunk-size-y 2) 0)))
    ; Amount of sun- or moonlight hitting each block, 4 bits per block.
    (getSkyLight [_]
                 (gen-dummy-data (repeat (/ chunk-size-y 2) (unchecked-byte 0xFF))))
    ; Amount of light emitted per block, 4 bits per block.
    (getBlockLight [_] (gen-dummy-data (repeat (/ chunk-size-y 2) 0)))
    ; Prepares a ChunkBlock from the data at the specified coordinates, which
    ; must exist within this chunk. If this chunk does not contain the
    ; specified coordinates, returns null.
    (getChunkBlock [_ blockX blockY blockZ]
                   (dummy-chunk-block blockX blockY blockZ
                                      (if (> blockY default-sea-level) (byte 0) (byte 1))
                                      (if (>= blockY default-sea-level) (byte 3) (byte 0))))
    ; The lowest level in each column in where the light from the sky
    ; is at full strength. This is arranged Z, X.
    (getHeightMap [_] (byte-array (repeat (* chunk-size-x chunk-size-z) default-sea-level)))
    ; Entities in the chunk.
    (getEntitities [_] [])
    ; Tile entities in the chunk.
    (getTileEntities [_] [])
    ; Tick when the chunk was last saved.
    (getLastUpdate [_] nil)
    ; X position of the chunk.
    (getXPos [_] x)
    ; Y position of the chunk.
    (getYPos [_] y)
    ; Z position of the chunk.
    (getZPos [_] z)
    ; Whether or not the terrain in this chunk was populated with
    ; special things. (Ores, special blocks, trees, dungeons, flowers,
    ; waterfalls, etc.)
    (isTerrainPopulated [_] true)
    ; ChunkWorld that is managing the chunk, null if it has not been assigned.
    (getWorld [_] chunk-world)))

(defn get-chunk [chunk-world world-data x y z]
  (reify Chunk
    ; Ids of the blocks in this chunk.
    ; Get the Blocks tag from the chunk map's NBT data 
    (getBlockIds [_] (retrieve-tag (:data (get-chunk-map world-data)) tag-blocks))
    ; Ancillary data for the blocks in this chunk, 4 bits per block.
    (getBlockData [_] (gen-dummy-data (repeat (/ chunk-size-y 2) 0)))
    ; Amount of sun- or moonlight hitting each block, 4 bits per block.
    (getSkyLight [_]
                 (gen-dummy-data (repeat (/ chunk-size-y 2) (unchecked-byte 0xFF))))
    ; Amount of light emitted per block, 4 bits per block.
    (getBlockLight [_] (gen-dummy-data (repeat (/ chunk-size-y 2) 0)))
    ; Prepares a ChunkBlock from the data at the specified coordinates, which
    ; must exist within this chunk. If this chunk does not contain the
    ; specified coordinates, returns null.
    (getChunkBlock [_ blockX blockY blockZ]
                   (dummy-chunk-block blockX blockY blockZ
                                      (if (> blockY default-sea-level) (byte 0) (byte 1))
                                      (if (>= blockY default-sea-level) (byte 3) (byte 0))))
    ; The lowest level in each column in where the light from the sky
    ; is at full strength. This is arranged Z, X.
    (getHeightMap [_] (byte-array (repeat (* chunk-size-x chunk-size-z) default-sea-level)))
    ; Entities in the chunk.
    (getEntitities [_] [])
    ; Tile entities in the chunk.
    (getTileEntities [_] [])
    ; Tick when the chunk was last saved.
    (getLastUpdate [_] nil)
    ; X position of the chunk.
    (getXPos [_] x)
    ; Y position of the chunk.
    (getYPos [_] y)
    ; Z position of the chunk.
    (getZPos [_] z)
    ; Whether or not the terrain in this chunk was populated with
    ; special things. (Ores, special blocks, trees, dungeons, flowers,
    ; waterfalls, etc.)
    (isTerrainPopulated [_] true)
    ; ChunkWorld that is managing the chunk, null if it has not been assigned.
    (getWorld [_] chunk-world)))

(defn -loadChunkWorld [this world-name]
  ; Read the origin chunk, if possible
  (let [world-data
        (if-let [save-dir (load-save-dir world-name)]
          (let [origin-descriptor (create-file-descriptor 0 0 0)]
            {[0 0] (read-region-file origin-descriptor ((:region-map save-dir) (:filename origin-descriptor)))})
        nil)]
    (if (nil? world-data)
      nil
      (reify ChunkWorld
        (hasChunk [this x y z]
                  (contains? world-data [x y]))
        (getChunk [this x y z] (if (contains? world-data [x y]) (get-chunk this (get world-data [x y]) x y z) nil))
        (getChunkSizeX [_] chunk-size-x)
        (getChunkSizeY [_] chunk-size-y)
        (getChunkSizeZ [_] chunk-size-z)))))