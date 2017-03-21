package com.microsoft.bond.protocol

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.Charset

class BinaryStreamWriter<out T : OutputStream>(private val stream: T) {
    companion object {
        private val UTF8 = Charset.forName("UTF-8")
        private val UTF16LE = Charset.forName("UTF-16LE")
    }

    private var position: Long = 0
    // Reusable buffer for primitive types.
    private var primitiveBuffer: ByteBuffer = ByteBuffer.allocate(8)
    private val varIntResult = VarIntHelper.Result()

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
        VarIntHelper.encodeVarInt16(value, varIntResult)
        stream.write(varIntResult.data, 0, varIntResult.length)
    }

    fun writeVarInt32(value: Int) {
        VarIntHelper.encodeVarInt32(value, varIntResult)
        stream.write(varIntResult.data, 0, varIntResult.length)
    }

    fun writeVarInt64(value: Long) {
        VarIntHelper.encodeVarInt64(value, varIntResult)
        stream.write(varIntResult.data, 0, varIntResult.length)
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

    fun writeString(value: String) {
        // FIXME: Make sure this generates null bytes and not \u0000.
        val encoded = value.toByteArray(UTF8)
        writeBytes(encoded)
    }

    fun writeWString(value: String) {
        val encoded = value.toByteArray(UTF16LE)
        writeBytes(encoded)
    }

    fun clone(): BinaryStreamWriter<T> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}
