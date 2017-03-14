package com.microsoft.bond.stream

import java.io.FileOutputStream
import java.io.OutputStream
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.ByteOrder

abstract class BondOutputStream(protected val stream: OutputStream, var position: Long = 0) {
    // Reusable buffer for primitive types.
    protected var primitiveBuffer: ByteBuffer = ByteBuffer.allocate(8)

    init {
        primitiveBuffer.order(ByteOrder.LITTLE_ENDIAN)
    }

    abstract fun writeInt8(value: Byte)

    abstract fun writeInt16(value: Short)

    abstract fun writeInt32(value: Int)

    abstract fun writeInt64(value: Long)

    abstract fun writeUInt8(value: Short)

    abstract fun writeUInt16(value: Int)

    abstract fun writeUInt32(value: Long)

    abstract fun writeUInt64(value: BigInteger)

    abstract fun writeFloat(value: Float)

    abstract fun writeDouble(value: Double)

    abstract fun writeBytes(value: ByteArray)

    abstract fun writeBool(value: Boolean)

    abstract fun writeString(value: String)

    abstract fun writeWString(value: String)

    abstract fun clone(): BondOutputStream
}

class BondFileOutputStream(path: String) : BondOutputStream(FileOutputStream(path)) {
    override fun writeInt8(value: Byte) {
        primitiveBuffer.put(0, value)
        stream.write(primitiveBuffer.array(), 0, 1)
    }

    override fun writeInt16(value: Short) {
        primitiveBuffer.putShort(0, value)
        stream.write(primitiveBuffer.array(), 0, 2)
    }

    override fun writeInt32(value: Int) {
        primitiveBuffer.putInt(0, value)
        stream.write(primitiveBuffer.array(), 0, 4)
    }

    override fun writeInt64(value: Long) {
        primitiveBuffer.putLong(0, value)
        stream.write(primitiveBuffer.array(), 0, 8)
    }

    override fun writeUInt8(value: Short) {
        writeInt8(value.toByte())
    }

    override fun writeUInt16(value: Int) {
        writeInt16(value.toShort())
    }

    override fun writeUInt32(value: Long) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeUInt64(value: BigInteger) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeFloat(value: Float) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeDouble(value: Double) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeBytes(value: ByteArray) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeBool(value: Boolean) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeString(value: String) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun writeWString(value: String) {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun clone(): BondOutputStream {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}
