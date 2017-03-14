package com.microsoft.bond.stream

import java.io.FileOutputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.Charset

abstract class BondOutputStream(protected val stream: OutputStream) {
    protected var position: Long = 0
    // Reusable buffer for primitive types.
    protected var primitiveBuffer: ByteBuffer = ByteBuffer.allocate(8)

    init {
        primitiveBuffer.order(ByteOrder.LITTLE_ENDIAN)
    }

    abstract fun writeInt8(value: Byte)

    abstract fun writeInt16(value: Short)

    abstract fun writeInt32(value: Int)

    abstract fun writeInt64(value: Long)

    abstract fun writeVarInt16(value: Short)

    abstract fun writeVarInt32(value: Int)

    abstract fun writeVarInt64(value: Long)

    abstract fun writeFloat(value: Float)

    abstract fun writeDouble(value: Double)

    abstract fun writeBytes(value: ByteArray)

    abstract fun writeBool(value: Boolean)

    abstract fun writeString(value: String, charset: Charset)

    abstract fun clone(): BondOutputStream
}

class BondFileOutputStream(path: String) : BondOutputStream(FileOutputStream(path)) {
    override fun writeInt8(value: Byte) {
        primitiveBuffer.put(0, value)
        stream.write(primitiveBuffer.array(), 0, 1)
        position += 1
    }

    override fun writeInt16(value: Short) {
        primitiveBuffer.putShort(0, value)
        stream.write(primitiveBuffer.array(), 0, 2)
        position += 2
    }

    override fun writeInt32(value: Int) {
        primitiveBuffer.putInt(0, value)
        stream.write(primitiveBuffer.array(), 0, 4)
        position += 4
    }

    override fun writeInt64(value: Long) {
        primitiveBuffer.putLong(0, value)
        stream.write(primitiveBuffer.array(), 0, 8)
        position += 8
    }

    override fun writeVarInt16(value: Short) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeVarInt32(value: Int) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeVarInt64(value: Long) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeFloat(value: Float) {
        primitiveBuffer.putFloat(0, value)
        stream.write(primitiveBuffer.array(), 0, 4)
        position += 4
    }

    override fun writeDouble(value: Double) {
        primitiveBuffer.putDouble(0, value)
        stream.write(primitiveBuffer.array(), 0, 8)
        position += 8
    }

    override fun writeBytes(value: ByteArray) {
        stream.write(value)
        position += value.size
    }

    override fun writeBool(value: Boolean) {
        writeInt8(if (value) 1 else 0)
    }

    override fun writeString(value: String, charset: Charset) {
        val encoded = value.toByteArray(charset)
        writeBytes(encoded)
    }

    override fun clone(): BondOutputStream {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}
