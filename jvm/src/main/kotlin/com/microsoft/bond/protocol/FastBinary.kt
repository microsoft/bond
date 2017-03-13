package com.microsoft.bond.protocol

import com.microsoft.bond.BondDataType
import com.microsoft.bond.Metadata
import com.microsoft.bond.ProtocolType
import java.io.DataOutputStream
import java.io.OutputStream
import java.math.BigInteger

class FastBinaryWriter<out O : OutputStream>(val output: O, val version: Short = 1) : ProtocolWriter {
    val Magic = ProtocolType.FAST_PROTOCOL
    val dataOutput = DataOutputStream(output)

    override fun writeVersion() {
        dataOutput.writeShort(Magic.value)
        dataOutput.writeShort(version.toInt())
    }

    override fun writeStructBegin(metadata: Metadata?) {}

    override fun writeBaseBegin(metadata: Metadata?) {}

    override fun writeStructEnd() {
        dataOutput.writeByte(BondDataType.BT_STOP.value)
    }

    override fun writeBaseEnd() {
        dataOutput.writeByte(BondDataType.BT_STOP_BASE.value)
    }

    override fun writeFieldBegin(type: BondDataType, id: Int, metadata: Metadata?) {
        dataOutput.writeByte(type.value)
        dataOutput.writeShort(id)
    }

    override fun writeFieldEnd() {}

    override fun writeFieldOmitted(type: BondDataType, id: Int, metadata: Metadata?) {}

    override fun writeContainerBegin(count: Int, elementType: BondDataType) {
        dataOutput.writeByte(elementType.value)
        dataOutput.writeInt(count)
    }

    override fun writeContainerBegin(count: Int, keyType: BondDataType, valueType: BondDataType) {
        dataOutput.writeByte(keyType.value)
        dataOutput.writeByte(valueType.value)
        dataOutput.writeInt(count)
    }

    override fun writeContainerEnd() {}

    override fun writeInt8(value: Byte) {
        dataOutput.writeByte(value.toInt())
    }

    override fun writeInt16(value: Short) {
        dataOutput.writeShort(value.toInt())
    }

    override fun writeInt32(value: Int) {
        dataOutput.writeInt(value)
    }

    override fun writeInt64(value: Long) {
        dataOutput.writeLong(value)
    }

    override fun writeUInt8(value: Short) {
        dataOutput.writeByte(value.toInt())
    }

    override fun writeUInt16(value: Int) {
        dataOutput.writeShort(value)
    }

    override fun writeUInt32(value: Long) {
        dataOutput.writeInt(value.toInt())
    }

    override fun writeUInt64(value: BigInteger) {
        dataOutput.writeLong(value.toLong())
    }

    override fun writeFloat(value: Float) {
        dataOutput.writeFloat(value)
    }

    override fun writeDouble(value: Double) {
        dataOutput.writeDouble(value)
    }

    override fun writeBytes(value: ByteArray) {
        dataOutput.write(value)
    }

    override fun writeBool(value: Boolean) {
        dataOutput.writeByte(if (value) 1 else 0)
    }

    override fun writeString(value: String) {
        dataOutput.writeInt(value.length)
        if (value.isNotEmpty()) {
            val bytes = value.toByteArray(Charsets.UTF_8)
            dataOutput.write(bytes)
        }
    }

    override fun writeWString(value: String) {
        dataOutput.writeInt(value.length)
        if (value.isNotEmpty()) {
            val bytes = value.toByteArray(Charsets.UTF_16LE)
            dataOutput.write(bytes)
        }
    }
}
