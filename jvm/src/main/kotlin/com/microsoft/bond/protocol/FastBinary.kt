package com.microsoft.bond.protocol

import com.microsoft.bond.BondDataType
import com.microsoft.bond.Metadata
import com.microsoft.bond.ProtocolType
import java.io.OutputStream
import java.math.BigInteger

class FastBinaryWriter<out S : OutputStream>(val stream: S, val version: Short = 1): ProtocolWriter {
    val Magic = ProtocolType.FAST_PROTOCOL
    private val output = BinaryStreamWriter(stream)

    override fun writeVersion() {
        output.writeInt16(Magic.value.toShort())
        output.writeInt16(version)
    }

    override fun writeStructBegin(metadata: Metadata?) {}

    override fun writeBaseBegin(metadata: Metadata?) {}

    override fun writeStructEnd() {
        output.writeInt8(BondDataType.BT_STOP.value.toByte())
    }

    override fun writeBaseEnd() {
        output.writeInt8(BondDataType.BT_STOP_BASE.value.toByte())
    }

    override fun writeFieldBegin(type: BondDataType, id: Int, metadata: Metadata?) {
        output.writeInt8(type.value.toByte())
        output.writeInt16(id.toShort())
    }

    override fun writeFieldEnd() {}

    override fun writeFieldOmitted(type: BondDataType, id: Int, metadata: Metadata?) {}

    override fun writeContainerBegin(count: Int, elementType: BondDataType) {
        output.writeInt8(elementType.value.toByte())
        output.writeInt32(count)
    }

    override fun writeContainerBegin(count: Int, keyType: BondDataType, valueType: BondDataType) {
        output.writeInt8(keyType.value.toByte())
        output.writeInt8(valueType.value.toByte())
        output.writeInt32(count)
    }

    override fun writeContainerEnd() {}

    override fun writeInt8(value: Byte) {
        output.writeInt8(value)
    }

    override fun writeInt16(value: Short) {
        output.writeInt16(value)
    }

    override fun writeInt32(value: Int) {
        output.writeInt32(value)
    }

    override fun writeInt64(value: Long) {
        output.writeInt64(value)
    }

    override fun writeUInt8(value: Short) {
        output.writeInt8(value.toByte())
    }

    override fun writeUInt16(value: Int) {
        output.writeInt16(value.toShort())
    }

    override fun writeUInt32(value: Long) {
        output.writeInt32(value.toInt())
    }

    override fun writeUInt64(value: BigInteger) {
        output.writeInt64(value.toLong())
    }

    override fun writeFloat(value: Float) {
        output.writeFloat(value)
    }

    override fun writeDouble(value: Double) {
        output.writeDouble(value)
    }

    override fun writeBytes(value: ByteArray) {
        output.writeBytes(value)
    }

    override fun writeBool(value: Boolean) {
        output.writeBool(value)
    }

    override fun writeString(value: String) {
        output.writeString(value)
    }

    override fun writeWString(value: String) {
        output.writeWString(value)
    }
}
