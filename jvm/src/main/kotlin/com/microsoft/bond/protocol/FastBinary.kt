package com.microsoft.bond.protocol

import com.microsoft.bond.BondDataType
import com.microsoft.bond.Metadata
import com.microsoft.bond.ProtocolType
import java.io.OutputStream
import java.math.BigInteger

class FastBinaryWriter<out S : OutputStream>(val stream: S, val version: Short = 1): ProtocolWriter {
    val Magic = ProtocolType.FAST_PROTOCOL
    private val writer = BinaryStreamWriter(stream)

    override fun writeVersion() {
        writer.writeInt16(Magic.value.toShort())
        writer.writeInt16(version)
    }

    override fun writeStructBegin(metadata: Metadata?) {}

    override fun writeBaseBegin(metadata: Metadata?) {}

    override fun writeStructEnd() {
        writer.writeInt8(BondDataType.BT_STOP.value.toByte())
    }

    override fun writeBaseEnd() {
        writer.writeInt8(BondDataType.BT_STOP_BASE.value.toByte())
    }

    override fun writeFieldBegin(type: BondDataType, id: Int, metadata: Metadata?) {
        writer.writeInt8(type.value.toByte())
        writer.writeInt16(id.toShort())
    }

    override fun writeFieldEnd() {}

    override fun writeFieldOmitted(type: BondDataType, id: Int, metadata: Metadata?) {}

    override fun writeContainerBegin(count: Int, elementType: BondDataType) {
        writer.writeInt8(elementType.value.toByte())
        writer.writeVarInt32(count)
    }

    override fun writeContainerBegin(count: Int, keyType: BondDataType, valueType: BondDataType) {
        writer.writeInt8(keyType.value.toByte())
        writer.writeInt8(valueType.value.toByte())
        writer.writeVarInt32(count)
    }

    override fun writeContainerEnd() {}

    override fun writeInt8(value: Byte) {
        writer.writeInt8(value)
    }

    override fun writeInt16(value: Short) {
        writer.writeInt16(value)
    }

    override fun writeInt32(value: Int) {
        writer.writeInt32(value)
    }

    override fun writeInt64(value: Long) {
        writer.writeInt64(value)
    }

    override fun writeUInt8(value: Short) {
        writer.writeInt8(value.toByte())
    }

    override fun writeUInt16(value: Int) {
        writer.writeInt16(value.toShort())
    }

    override fun writeUInt32(value: Long) {
        writer.writeInt32(value.toInt())
    }

    override fun writeUInt64(value: BigInteger) {
        writer.writeInt64(value.toLong())
    }

    override fun writeFloat(value: Float) {
        writer.writeFloat(value)
    }

    override fun writeDouble(value: Double) {
        writer.writeDouble(value)
    }

    override fun writeBytes(value: ByteArray) {
        writer.writeBytes(value)
    }

    override fun writeBool(value: Boolean) {
        writer.writeBool(value)
    }

    override fun writeString(value: String) {
        writer.writeVarInt32(value.length)
        writer.writeString(value)
    }

    override fun writeWString(value: String) {
        writer.writeVarInt32(value.length)
        writer.writeWString(value)
    }
}
