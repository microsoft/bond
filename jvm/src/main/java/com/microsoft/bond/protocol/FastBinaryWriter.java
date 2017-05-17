package com.microsoft.bond.protocol;

import com.microsoft.bond.BondDataType;
import com.microsoft.bond.Metadata;
import com.microsoft.bond.ProtocolType;

import java.io.IOException;
import java.io.OutputStream;

public final class FastBinaryWriter<S extends OutputStream> implements ProtocolWriter {
    private static final ProtocolType MAGIC = ProtocolType.FAST_PROTOCOL;

    public final S stream;
    private final short version;
    private final BinaryStreamWriter writer;

    public FastBinaryWriter(final S stream, final short version) {
        if (stream == null) {
            throw new IllegalArgumentException("stream cannot be null");
        }
        if (version != 1) {
            throw new IllegalArgumentException("invalid version " + version);
        }

        this.stream = stream;
        this.version = version;
        this.writer = new BinaryStreamWriter(stream);
    }

    @Override
    public ProtocolWriter getFirstPassWriter() {
        return null;
    }

    @Override
    public void writeVersion() throws IOException {
        writer.writeInt16((short) MAGIC.value);
        writer.writeInt16(version);
    }

    @Override
    public void writeStructBegin(final Metadata metadata) throws IOException {}

    @Override
    public void writeBaseBegin(final Metadata metadata) throws IOException {}

    @Override
    public void writeStructEnd() throws IOException {
        writer.writeInt8((byte) BondDataType.BT_STOP.value);
    }

    @Override
    public void writeBaseEnd() throws IOException {
        writer.writeInt8((byte) BondDataType.BT_STOP_BASE.value);
    }

    @Override
    public void writeFieldBegin(final BondDataType type, final int id, final Metadata metadata) throws IOException {
        writer.writeInt8((byte) type.value);
        writer.writeInt16((short) id);
    }

    @Override
    public void writeFieldEnd() throws IOException {}

    @Override
    public void writeFieldOmitted(final BondDataType type, final int id, final Metadata metadata) throws IOException {}

    @Override
    public void writeContainerBegin(final int count, final BondDataType elementType) throws IOException {
        writer.writeInt8((byte) elementType.value);
        writer.writeVarUInt32(count);
    }

    @Override
    public void writeContainerBegin(final int count, final BondDataType keyType, final BondDataType valueType)
        throws IOException {
        writer.writeInt8((byte) keyType.value);
        writer.writeInt8((byte) valueType.value);
        writer.writeVarUInt32(count);
    }

    @Override
    public void writeContainerEnd() throws IOException {}

    @Override
    public void writeInt8(final byte value) throws IOException {
        writer.writeInt8(value);
    }

    @Override
    public void writeInt16(final short value) throws IOException {
        writer.writeInt16(value);
    }

    @Override
    public void writeInt32(final int value) throws IOException {
        writer.writeInt32(value);
    }

    @Override
    public void writeInt64(final long value) throws IOException {
        writer.writeInt64(value);
    }

    @Override
    public void writeUInt8(final byte value) throws IOException {
        writer.writeInt8(value);
    }

    @Override
    public void writeUInt16(final short value) throws IOException {
        writer.writeInt16(value);
    }

    @Override
    public void writeUInt32(final int value) throws IOException {
        writer.writeInt32(value);
    }

    @Override
    public void writeUInt64(final long value) throws IOException {
        writer.writeInt64(value);
    }

    @Override
    public void writeFloat(final float value) throws IOException {
        writer.writeFloat(value);
    }

    @Override
    public void writeDouble(final double value) throws IOException {
        writer.writeDouble(value);
    }

    @Override
    public void writeBytes(final byte[] value) throws IOException {
        writer.writeBytes(value);
    }

    @Override
    public void writeBool(final boolean value) throws IOException {
        writer.writeBool(value);
    }

    @Override
    public void writeString(final String value) throws IOException {
        final byte[] bytes = StringHelper.encodeString(value);
        writer.writeVarUInt32(bytes.length);
        writer.writeBytes(bytes);
    }

    @Override
    public void writeWString(final String value) throws IOException {
        final byte[] bytes = StringHelper.encodeWString(value);
        writer.writeVarUInt32(bytes.length / 2);
        writer.writeBytes(bytes);
    }
}
