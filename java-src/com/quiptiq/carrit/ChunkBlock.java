package com.quiptiq.carrit;

/**
 * Block in a chunk map.
 * 
 * @author Taufiq Hoven
 */
public interface ChunkBlock {
    /**
     * @return Id of the type of block.
     */
    byte getBlockTypeId();

    /**
     * @return Ancillary data for the block. The exact nature of this data will
     *         depend on the block type.
     */
    byte getBlockData();

    /**
     * @return Amount of light emitted by this block.
     */
    byte getLightEmitted();

    /**
     * @return Amount of light from the sky that is received by this block.
     */
    byte getSkyLight();

    /**
     * @return X coordinate of this block.
     */
    int getX();

    /**
     * @return Y coordinate of this block.
     */
    int getY();

    /**
     * @return Z coordinate of this block.
     */
    int getZ();
}
