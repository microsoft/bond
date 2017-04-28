package com.microsoft.bond.protocol

import com.microsoft.bond.exception.EndOfStreamException
import java.io.BufferedInputStream
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.Charset

class BinaryStreamReader<out T : InputStream>(private val stream: T) {
    companion object {
        private val UTF8 = Charset.forName("UTF-8")
        private val UTF16LE = Charset.forName("UTF-16LE")
    }

    // We need a BufferedStream for the current implementation of varints.
    // See ::readVarInt below.
    private var bufferedStream = BufferedInputStream(stream)
    private var position = 0
    // Reusable buffer for primitive types.
    // Must be large enough to hold the worst value of a 64-bit varint.
    private var primitiveBuffer: ByteBuffer = ByteBuffer.allocate(VarIntHelper.VarInt64MaxBytes)
    private val varIntDecodeResult = VarIntHelper.DecodeResult()

    init {
        primitiveBuffer.order(ByteOrder.LITTLE_ENDIAN)
    }

    private fun readPrimitive(length: Int) {
        if (bufferedStream.read(primitiveBuffer.array(), 0, length) < length) {
            throw EndOfStreamException()
        }
        position += length
    }

    fun readInt8(): Byte {
        readPrimitive(1)
        return primitiveBuffer.get(0)
    }

    fun readInt16(): Short {
        readPrimitive(2)
        return primitiveBuffer.getShort(0)
    }

    fun readInt32(): Int {
        readPrimitive(4)
        return primitiveBuffer.getInt(0)
    }

    fun readInt64(): Long {
        readPrimitive(8)
        return primitiveBuffer.getLong(0)
    }

    /**
     * VarIntHelper is naive and needs to be given VarIntNMaxBytes of input to
     * decode, but if it ends up not using all of them, InputStream doesn't
     * know how to rewind. A BufferedInputStream is the simplest answer, but
     * may not be the best. We should look into a smarter varint decoder that
     * can read a byte at a time.
     */
    private fun readVarInt(maxBytes: Int, decode: (ByteArray, Int, VarIntHelper.DecodeResult) -> Unit) {
        bufferedStream.mark(position)

        readPrimitive(maxBytes)
        decode(primitiveBuffer.array(), 0, varIntDecodeResult)

        bufferedStream.reset()
        bufferedStream.skip(varIntDecodeResult.length.toLong())

        // How many bytes do we need to "give back" to the stream?
        val overreadBy = maxBytes - varIntDecodeResult.length
        position -= overreadBy
    }

    fun readVarInt16(): Short {
        readVarInt(VarIntHelper.VarInt16MaxBytes, VarIntHelper::decodeVarInt16)
        return varIntDecodeResult.value.toShort()
    }

    fun readVarInt32(): Int {
        readVarInt(VarIntHelper.VarInt32MaxBytes, VarIntHelper::decodeVarInt32)
        return varIntDecodeResult.value.toInt()
    }

    fun readVarInt64(): Long {
        readVarInt(VarIntHelper.VarInt64MaxBytes, VarIntHelper::decodeVarInt64)
        return varIntDecodeResult.value
    }

    fun readFloat(): Float {
        readPrimitive(4)
        return primitiveBuffer.getFloat(0)
    }

    fun readDouble(): Double {
        readPrimitive(8)
        return primitiveBuffer.getDouble(0)
    }

    fun readBytes(length: Int): ByteArray {
        val ret = ByteArray(length)
        if (bufferedStream.read(ret, 0, length) < length) {
            throw EndOfStreamException()
        }
        position += length
        return ret
    }

    fun readBool(): Boolean = readInt8() != 0.toByte()

    fun readString(length: Int): String {
        val bytes = readBytes(length)
        return String(bytes, UTF8)
    }

    fun readWString(length:Int): String {
        val bytes = readBytes(length)
        return String(bytes, UTF16LE)
    }

    fun clone(): BinaryStreamReader<T> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}
