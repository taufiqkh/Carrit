package com.quiptiq.carrit;

import java.util.List;

/**
 * Represents a chunk in a map.
 *
 * NOTE: This is a temporary interface, until associated types have been written
 * for better representing block information.
 *
 * Block information is arranged such that:
 *
 * <pre>
 * BlockID = Blocks[ y + z * ChunkSizeY(=128) + x * ChunkSizeY(=128) * ChunkSizeZ(=16) ]
 * </pre>
 *
 * @author Taufiq Hoven
 */
public interface Chunk {
    /**
     * @return  Ids of the blocks in this chunk.
     */
    byte[] getBlockIds();

    /**
     * @return  Ancillary data for the blocks in this chunk, 4 bits per block.
     */
    byte[] getBlockData();

    /**
     * @return Amount of sun- or moonlight hitting each block, 4 bits per block.
     */
    byte[] getSkyLight();

    /**
     * @return Amount of light emitted per block, 4 bits per block.
     */
    byte[] getBlockLight();

    /**
     * @return The lowest level in each column in where the light from the sky
     *         is at full strength. This is arranged Z, X.
     */
    byte[] getHeightMap();

    /**
     * @return  Entities in the chunk.
     */
    List<Object> getEntitities();

    /**
     * @return  Tile entities in the chunk.
     */
    List<Object> getTileEntities();

    /**
     * @return  Tick when the chunk was last saved.
     */
    long getLastUpdate();

    /**
     * @return X position of the chunk.
     */
    int getXPos();

    /**
     * @return Y position of the chunk.
     */
    int getYPos();

    /**
     * @return Z position of the chunk.
     */
    int getZPos();

    /**
     * @return Whether or not the terrain in this chunk was populated with
     *         special things. (Ores, special blocks, trees, dungeons, flowers,
     *         waterfalls, etc.)
     */
    boolean isTerrainPopulated();

    /**
     * @return  ChunkWorld that is managing the chunk, null if it has not been assigned.
     */
    ChunkWorld getWorld();
}
