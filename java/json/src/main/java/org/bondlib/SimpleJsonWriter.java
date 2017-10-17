// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import com.fasterxml.jackson.core.*;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Implements Simple JSON protocol writer for object serialization.
 */
public final class SimpleJsonWriter implements ProtocolWriter {

    private final JsonGenerator generator;

    public SimpleJsonWriter(OutputStream outputStream) throws IOException {
        ArgumentHelper.ensureNotNull(outputStream, "outputStream");
        this.generator = JsonGlobals.jsonFactory.createGenerator(outputStream);
    }

    @Override
    public final boolean usesMarshaledBonded() {
        return false;
    }

    @Override
    public final void writeVersion() throws IOException {
        // this protocol does not support marshalling
    }

    @Override
    public final void writeStructBegin(Metadata metadata) throws IOException {
        this.generator.writeStartObject();
    }

    @Override
    public final void writeStructEnd() throws IOException {
        this.generator.writeEndObject();
    }

    @Override
    public final void writeBaseBegin(Metadata metadata) throws IOException {
        // this protocol flattens type hierarchy
    }

    @Override
    public final void writeBaseEnd() throws IOException {
        // this protocol flattens type hierarchy
    }

    @Override
    public final void writeFieldBegin(BondDataType bondDataType, int i, Metadata metadata) throws IOException {
        this.generator.writeFieldName(metadata.name);
    }

    @Override
    public final void writeFieldEnd() throws IOException {
    }

    @Override
    public final void writeFieldOmitted(BondDataType bondDataType, int i, Metadata metadata) throws IOException {
    }

    @Override
    public final void writeContainerBegin(int i, BondDataType bondDataType) throws IOException {
        this.generator.writeStartArray();
    }

    @Override
    public final void writeContainerBegin(int i, BondDataType bondDataType, BondDataType bondDataType1) throws IOException {
        this.generator.writeStartArray();
    }

    @Override
    public void writeContainerEnd() throws IOException {
        this.generator.writeEndArray();
    }

    @Override
    public final void writeInt8(byte b) throws IOException {
        this.generator.writeNumber(b);
    }

    @Override
    public final void writeInt16(short i) throws IOException {
        this.generator.writeNumber(i);
    }

    @Override
    public final void writeInt32(int i) throws IOException {
        this.generator.writeNumber(i);
    }

    @Override
    public final void writeInt64(long l) throws IOException {
        this.generator.writeNumber(l);
    }

    @Override
    public final void writeUInt8(byte b) throws IOException {
        this.generator.writeNumber(UnsignedHelper.asUnsignedShort(b));
    }

    @Override
    public final void writeUInt16(short i) throws IOException {
        this.generator.writeNumber(UnsignedHelper.asUnsignedInt(i));
    }

    @Override
    public final void writeUInt32(int i) throws IOException {
        this.generator.writeNumber(UnsignedHelper.asUnsignedLong(i));
    }

    @Override
    public final void writeUInt64(long l) throws IOException {
        this.generator.writeNumber(UnsignedHelper.asUnsignedBigInt(l));
    }

    @Override
    public final void writeFloat(float v) throws IOException {
        this.generator.writeNumber(v);
    }

    @Override
    public final void writeDouble(double v) throws IOException {
        this.generator.writeNumber(v);
    }

    @Override
    public final void writeBytes(byte[] bytes) throws IOException {
        this.generator.writeStartArray();
        for (int i = 0; i < bytes.length; ++i) {
            this.generator.writeNumber(bytes[i]);
        }
        this.generator.writeEndArray();
    }

    @Override
    public final void writeBool(boolean b) throws IOException {
        this.generator.writeBoolean(b);
    }

    @Override
    public final void writeString(String s) throws IOException {
        this.generator.writeString(s);
    }

    @Override
    public final void writeWString(String s) throws IOException {
        this.generator.writeString(s);
    }
}
