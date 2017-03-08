package com.microsoft.bond

/**
 * Writes a serialized payload.
 */
interface ProtocolWriter {
    /**
     * Write a protocol magic number and version.
     */
    fun writeVersion()

    fun writeStructBegin(metadata: Metadata)
}
