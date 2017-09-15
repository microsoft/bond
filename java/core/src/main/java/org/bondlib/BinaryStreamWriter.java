// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * Responsible for writing Bond primitive data types to an output stream.
 * This class encapsulates big-endian to little-endian conversion and Bool encoding.
 */
final class BinaryStreamWriter {

    private final OutputStream outputStream;
    private final ByteBuffer endiannessConversionByteBuffer;

    BinaryStreamWriter(OutputStream outputStream) {
        this.outputStream = outputStream;
        this.endiannessConversionByteBuffer = ByteBuffer.allocate(8);
        this.endiannessConversionByteBuffer.order(ByteOrder.LITTLE_ENDIAN);
    }

    void writeInt8(byte value) throws IOException {
        this.outputStream.write(value);
    }

    void writeInt16(short value) throws IOException {
        this.endiannessConversionByteBuffer.putShort(0, value);
        this.outputStream.write(this.endiannessConversionByteBuffer.array(), 0, 2);
    }

    void writeInt32(int value) throws IOException {
        this.endiannessConversionByteBuffer.putInt(0, value);
        this.outputStream.write(this.endiannessConversionByteBuffer.array(), 0, 4);
    }

    void writeInt64(long value) throws IOException {
        this.endiannessConversionByteBuffer.putLong(0, value);
        this.outputStream.write(this.endiannessConversionByteBuffer.array(), 0, 8);
    }

    void writeFloat(float value) throws IOException {
        this.endiannessConversionByteBuffer.putFloat(0, value);
        this.outputStream.write(this.endiannessConversionByteBuffer.array(), 0, 4);
    }

    void writeDouble(double value) throws IOException {
        this.endiannessConversionByteBuffer.putDouble(0, value);
        this.outputStream.write(this.endiannessConversionByteBuffer.array(), 0, 8);
    }

    void writeBool(boolean value) throws IOException {
        this.outputStream.write(value ? 1 : 0);
    }

    void writeBytes(byte[] value) throws IOException {
        this.outputStream.write(value);
    }

    void writeVarUInt16(short value) throws IOException {
        VarUIntHelper.encodeVarUInt16(value, this.outputStream);
    }

    void writeVarUInt32(int value) throws IOException {
        VarUIntHelper.encodeVarUInt32(value, this.outputStream);
    }

    void writeVarUInt64(long value) throws IOException {
        VarUIntHelper.encodeVarUInt64(value, this.outputStream);
    }
}
