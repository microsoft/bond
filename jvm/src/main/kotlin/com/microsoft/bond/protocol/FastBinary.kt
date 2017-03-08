package com.microsoft.bond.protocol

import com.microsoft.bond.BondDataType
import com.microsoft.bond.Metadata
import com.microsoft.bond.ProtocolType
import java.io.DataOutputStream
import java.math.BigInteger

class FastBinaryWriter<out O : DataOutputStream>(val output: O, val version: Short = 1) : ProtocolWriter {
    val Magic = ProtocolType.FAST_PROTOCOL

    override fun writeVersion() {
        output.writeShort(Magic.ordinal)
        output.writeShort(version.toInt())
    }

    override fun writeStructBegin(metadata: Metadata) {}

    override fun writeBaseBegin(metadata: Metadata) {}

    override fun writeStructEnd() {
        output.writeByte(BondDataType.BT_STOP.ordinal)
    }

    override fun writeBaseEnd() {
        output.writeByte(BondDataType.BT_STOP_BASE.ordinal)
    }

    override fun writeFieldBegin(type: BondDataType, id: Int, metadata: Metadata) {
        output.writeByte(type.ordinal)
        output.writeShort(id)
    }

    override fun writeFieldEnd() {}

    override fun writeFieldOmitted(type: BondDataType, id: Int, metadata: Metadata) {}

    override fun writeContainerBegin(count: Int, elementType: BondDataType) {
        output.writeByte(elementType.ordinal)
        output.writeInt(count)
    }

    override fun writeContainerBegin(count: Int, keyType: BondDataType, valueType: BondDataType) {
        output.writeByte(keyType.ordinal)
        output.writeByte(valueType.ordinal)
        output.writeInt(count)
    }

    override fun writeContainerEnd() {}

    override fun writeInt8(value: Byte) {
        output.writeByte(value.toInt())
    }

    override fun writeInt16(value: Short) {
        output.writeShort(value.toInt())
    }

    override fun writeInt32(value: Int) {
        output.writeInt(value)
    }

    override fun writeInt64(value: Long) {
        output.writeLong(value)
    }

    override fun writeUInt8(value: Short) {
        output.writeByte(value.toInt())
    }

    override fun writeUInt16(value: Int) {
        output.writeShort(value)
    }

    override fun writeUInt32(value: Long) {
        output.writeInt(value.toInt())
    }

    override fun writeUInt64(value: BigInteger) {
        output.writeLong(value.toLong())
    }

    override fun writeFloat(value: Float) {
        output.writeFloat(value)
    }

    override fun writeDouble(value: Double) {
        output.writeDouble(value)
    }

    override fun writeBytes(value: ByteArray) {
        output.write(value)
    }

    override fun writeBool(value: Boolean) {
        output.writeByte(if (value) 1 else 0)
    }

    override fun writeString(value: String) {
        output.writeInt(value.length)
        if (value.isNotEmpty()) {
            val bytes = value.toByteArray(Charsets.UTF_8)
            output.write(bytes)
        }
    }

    override fun writeWString(value: String) {
        output.writeInt(value.length)
        if (value.isNotEmpty()) {
            val bytes = value.toByteArray(Charsets.UTF_16LE)
            output.write(bytes)
        }
    }
}
