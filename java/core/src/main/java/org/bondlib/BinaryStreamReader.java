// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * Responsible for reading Bond primitive data types from an input stream.
 * This class encapsulates little-endian to big-endian conversion and Bool encoding.
 */
final class BinaryStreamReader {

    final InputStream inputStream;
    private final ByteBuffer endiannessConversionByteBuffer;

    BinaryStreamReader(InputStream inputStream) {
        this.inputStream = inputStream;
        this.endiannessConversionByteBuffer = ByteBuffer.allocate(8);
        this.endiannessConversionByteBuffer.order(ByteOrder.LITTLE_ENDIAN);
    }

    byte readInt8() throws IOException {
        return StreamHelper.readByte(this.inputStream);
    }

    short readInt16() throws IOException {
        StreamHelper.readBytes(this.inputStream, this.endiannessConversionByteBuffer.array(), 0, 2);
        return this.endiannessConversionByteBuffer.getShort(0);
    }

    int readInt32() throws IOException {
        StreamHelper.readBytes(this.inputStream, this.endiannessConversionByteBuffer.array(), 0, 4);
        return this.endiannessConversionByteBuffer.getInt(0);
    }

    long readInt64() throws IOException {
        StreamHelper.readBytes(this.inputStream, this.endiannessConversionByteBuffer.array(), 0, 8);
        return this.endiannessConversionByteBuffer.getLong(0);
    }

    float readFloat() throws IOException {
        StreamHelper.readBytes(this.inputStream, this.endiannessConversionByteBuffer.array(), 0, 4);
        return this.endiannessConversionByteBuffer.getFloat(0);
    }

    double readDouble() throws IOException {
        StreamHelper.readBytes(this.inputStream, this.endiannessConversionByteBuffer.array(), 0, 8);
        return this.endiannessConversionByteBuffer.getDouble(0);
    }

    boolean readBool() throws IOException {
        return StreamHelper.readByte(this.inputStream) != 0;
    }

    byte[] readBytes(int byteCount) throws IOException {
        byte[] bytes = new byte[byteCount];
        StreamHelper.readBytes(this.inputStream, bytes, 0, byteCount);
        return bytes;
    }

    short readVarUInt16() throws IOException {
        return VarUIntHelper.decodeVarUInt16(this.inputStream);
    }

    int readVarUInt32() throws IOException {
        return VarUIntHelper.decodeVarUInt32(this.inputStream);
    }

    long readVarUInt64() throws IOException {
        return VarUIntHelper.decodeVarUInt64(this.inputStream);
    }

    void skipBytes(long byteCount) throws IOException {
        StreamHelper.skipBytes(this.inputStream, byteCount);
    }
}
