package com.quiptiq.carrit;

/**
 * Java API for the carrit chunk world.
 *
 * @author Taufiq Hoven
 */
public interface ChunkWorld {
    /**
     * Determine whether or not the chunk at the specified coordinates has been
     * generated and is currently managed.
     *
     * @param x
     *            X coordinate of the chunk.
     * @param y
     *            Y coordinate of the chunk.
     * @param z
     *            Z coordinate of the chunk.
     * @return True if the chunk has been generated and is currently under
     *         management, otherwise false.
     */
    boolean hasChunk(int x, int y, int z);

    /**
     * Gets the chunk at the specified coordinates, if it exists in management.
     *
     * @param x
     *            X coordinate of the chunk.
     * @param y
     *            Y coordinate of the chunk.
     * @param z
     *            Z coordinate of the chunk.
     * @return The chunk at the specified coordinates, or null if the chunk does
     *         not yet exist under management.
     */
    Chunk getChunk(int x, int y, int z);

    /**
     * @return Number of blocks in each chunk along the X axis.
     */
    int getChunkSizeX();

    /**
     * @return Number of blocks in each chunk along the Y axis.
     */
    int getChunkSizeY();

    /**
     * @return Number of blocks in each chunk along the Z axis.
     */
    int getChunkSizeZ();
}
