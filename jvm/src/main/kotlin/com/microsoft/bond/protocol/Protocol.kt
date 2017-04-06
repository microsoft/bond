package com.microsoft.bond.protocol

import com.microsoft.bond.BondDataType
import com.microsoft.bond.Metadata
import java.math.BigInteger

/**
 * Writes a serialized payload.
 */
interface ProtocolWriter {
    /**
     * Write a protocol magic number and version.
     */
    fun writeVersion()

    fun writeStructBegin(metadata: Metadata)

    fun writeBaseBegin(metadata: Metadata)

    fun writeStructEnd()

    fun writeBaseEnd()

    fun writeFieldBegin(type: BondDataType, id: Int, metadata: Metadata?)

    fun writeFieldEnd()

    /**
     * Indicate that a field was omitted because it was set to its default value.
     */
    fun writeFieldOmitted(type: BondDataType, id: Int, metadata: Metadata?)

    /**
     * Start writing a list or set container.
     */
    fun writeContainerBegin(count: Int, elementType: BondDataType)

    /**
     * Start writing a map container.
     */
    fun writeContainerBegin(count: Int, keyType: BondDataType, valueType: BondDataType)

    fun writeContainerEnd()

    fun writeInt8(value: Byte)

    fun writeInt16(value: Short)

    fun writeInt32(value: Int)

    fun writeInt64(value: Long)

    fun writeUInt8(value: Short)

    fun writeUInt16(value: Int)

    fun writeUInt32(value: Long)

    fun writeUInt64(value: BigInteger)

    fun writeFloat(value: Float)

    fun writeDouble(value: Double)

    fun writeBytes(value: ByteArray)

    fun writeBool(value: Boolean)

    fun writeString(value: String)

    fun writeWString(value: String)
}

/**
 * Provides an interface for protocols like Compact Binary v2 which need two passes.
 */
interface TwoPassProtocolWriter : ProtocolWriter {
    /**
     * Provide the first-pass writer, if one is needed. Otherwise, returns null.
     */
    fun getFirstPassWriter(): ProtocolWriter
}
