package com.microsoft.bond.protocol

import com.microsoft.bond.BondDataType
import com.microsoft.bond.Metadata
import com.microsoft.bond.ProtocolType
import java.io.InputStream
import java.io.OutputStream

class FastBinaryWriter<out S : OutputStream>(val stream: S, val version: Short = 1): ProtocolWriter {
    val Magic = ProtocolType.FAST_PROTOCOL
    private val writer = BinaryStreamWriter(stream)

    override fun writeVersion() {
        writer.writeInt16(Magic.value.toShort())
        writer.writeInt16(version)
    }

    override fun writeStructBegin(metadata: Metadata) {}

    override fun writeBaseBegin(metadata: Metadata) {}

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

    override fun writeUInt8(value: Byte) {
        writer.writeInt8(value)
    }

    override fun writeUInt16(value: Short) {
        writer.writeInt16(value)
    }

    override fun writeUInt32(value: Int) {
        writer.writeInt32(value)
    }

    override fun writeUInt64(value: Long) {
        writer.writeInt64(value)
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

class FastBinaryReader<out S : InputStream>(val stream: S, val version: Short = 1): TaggedProtocolReader {
    private val reader = BinaryStreamReader(stream)

    private fun readType(): BondDataType {
        return BondDataType(reader.readInt8().toInt())
    }

    override fun readStructBegin() {}

    override fun readBaseBegin() {}

    override fun readStructEnd() {}

    override fun readBaseEnd() {}

    override fun readFieldBegin(result: TaggedProtocolReader.ReadFieldResult) {
        result.type = readType()
        if (result.type != BondDataType.BT_STOP && result.type != BondDataType.BT_STOP_BASE) {
            result.id = UnsignedHelper.asUnsignedInt(reader.readInt16())
        } else {
            result.id = 0
        }
    }

    override fun readFieldEnd() {}

    override fun readListBegin(readContainerResult: TaggedProtocolReader.ReadContainerResult) {
        readContainerResult.key = null
        readContainerResult.element = readType()
        readContainerResult.count = UnsignedHelper.asUnsignedLong(reader.readVarInt32())
    }

    override fun readMapBegin(readContainerResult: TaggedProtocolReader.ReadContainerResult) {
        readContainerResult.key = readType()
        readContainerResult.element = readType()
        readContainerResult.count = UnsignedHelper.asUnsignedLong(reader.readVarInt32())
    }

    override fun readContainerEnd() {}

    override fun readInt8(): Byte {
        return reader.readInt8()
    }

    override fun readInt16(): Short {
        return reader.readInt16()
    }

    override fun readInt32(): Int {
        return reader.readInt32()
    }

    override fun readInt64(): Long {
        return reader.readInt64()
    }

    override fun readUInt8(): Byte {
        return readInt8()
    }

    override fun readUInt16(): Short {
        return readInt16()
    }

    override fun readUInt32(): Int {
        return readInt32()
    }

    override fun readUInt64(): Long {
        return readInt64()
    }

    override fun readFloat(): Float {
        return reader.readFloat()
    }

    override fun readDouble(): Double {
        return reader.readDouble()
    }

    override fun readBytes(): ByteArray {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun readBool(): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun readString(): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun readWString(): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun skip(type: BondDataType) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}
