package com.microsoft.bond.stream

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.Charset

class BondOutputStream<out T : OutputStream>(private val stream: T) {
    var position: Long = 0
    // Reusable buffer for primitive types.
    private var primitiveBuffer: ByteBuffer = ByteBuffer.allocate(8)

    init {
        primitiveBuffer.order(ByteOrder.LITTLE_ENDIAN)
    }

    fun writeInt8(value: Byte) {
        primitiveBuffer.put(0, value)
        stream.write(primitiveBuffer.array(), 0, 1)
        position += 1
    }

    fun writeInt16(value: Short) {
        primitiveBuffer.putShort(0, value)
        stream.write(primitiveBuffer.array(), 0, 2)
        position += 2
    }

    fun writeInt32(value: Int) {
        primitiveBuffer.putInt(0, value)
        stream.write(primitiveBuffer.array(), 0, 4)
        position += 4
    }

    fun writeInt64(value: Long) {
        primitiveBuffer.putLong(0, value)
        stream.write(primitiveBuffer.array(), 0, 8)
        position += 8
    }

    fun writeVarInt16(value: Short) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    fun writeVarInt32(value: Int) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    fun writeVarInt64(value: Long) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    fun writeFloat(value: Float) {
        primitiveBuffer.putFloat(0, value)
        stream.write(primitiveBuffer.array(), 0, 4)
        position += 4
    }

    fun writeDouble(value: Double) {
        primitiveBuffer.putDouble(0, value)
        stream.write(primitiveBuffer.array(), 0, 8)
        position += 8
    }

    fun writeBytes(value: ByteArray) {
        stream.write(value)
        position += value.size
    }

    fun writeBool(value: Boolean) {
        writeInt8(if (value) 1 else 0)
    }

    fun writeString(value: String, charset: Charset) {
        val encoded = value.toByteArray(charset)
        writeBytes(encoded)
    }

    fun clone(): BondOutputStream<T> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}
