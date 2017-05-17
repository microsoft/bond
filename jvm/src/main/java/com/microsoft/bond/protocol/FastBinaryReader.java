package com.microsoft.bond.protocol;

import com.microsoft.bond.BondDataType;

import java.io.IOException;
import java.io.InputStream;

/**
 * Implements Fast Binary deserialization.
 * Refer to {@link https://microsoft.github.io/bond/reference/cpp/fast__binary_8h_source.html} for details.
 */
public final class FastBinaryReader<S extends InputStream> implements TaggedProtocolReader {
    public final S stream;
    private final short version;
    private final BinaryStreamReader reader;

    public FastBinaryReader(final S stream, final short version) {
        if (stream == null) {
            throw new IllegalArgumentException("stream cannot be null");
        }
        if (version != 1) {
            throw new IllegalArgumentException("invalid version " + version);
        }

        this.stream = stream;
        this.version = version;
        this.reader = new BinaryStreamReader(stream);
    }

    private BondDataType readType() throws IOException {
        return new BondDataType(readInt8());
    }

    @Override
    public void readStructBegin() throws IOException {}

    @Override
    public void readBaseBegin() throws IOException {}

    @Override
    public void readStructEnd() throws IOException {
        // FIXME: Verify that this is BT_STOP.
        readInt8();
    }

    @Override
    public void readBaseEnd() throws IOException {
        // FIXME: Verify that this is BT_STOP_BASE.
        readInt8();
    }

    @Override
    public void readFieldBegin(ReadFieldResult result) throws IOException {
        final BondDataType type = readType();
        if (!type.equals(BondDataType.BT_STOP) && !type.equals(BondDataType.BT_STOP_BASE)) {
            result.id = UnsignedHelper.asUnsignedInt(reader.readInt16());
        } else {
            result.id = 0;
        }
        result.type = type;
    }

    @Override
    public void readFieldEnd() throws IOException {}

    @Override
    public void readListBegin(ReadContainerResult result) throws IOException {
        result.keyType = null;
        result.elementType = readType();
        result.count = reader.readVarUInt32();
    }

    @Override
    public void readMapBegin(ReadContainerResult result) throws IOException {
        result.keyType = readType();
        result.elementType = readType();
        result.count = reader.readVarUInt32();
    }

    @Override
    public void readContainerEnd() throws IOException {}

    @Override
    public byte readInt8() throws IOException {
        return reader.readInt8();
    }

    @Override
    public short readInt16() throws IOException {
        return reader.readInt16();
    }

    @Override
    public int readInt32() throws IOException {
        return reader.readInt32();
    }

    @Override
    public long readInt64() throws IOException {
        return reader.readInt64();
    }

    @Override
    public byte readUInt8() throws IOException {
        return reader.readInt8();
    }

    @Override
    public short readUInt16() throws IOException {
        return reader.readInt16();
    }

    @Override
    public int readUInt32() throws IOException {
        return reader.readInt32();
    }

    @Override
    public long readUInt64() throws IOException {
        return reader.readInt64();
    }

    @Override
    public float readFloat() throws IOException {
        return reader.readFloat();
    }

    @Override
    public double readDouble() throws IOException {
        return reader.readDouble();
    }

    @Override
    public byte[] readBytes(int count) throws IOException {
        return reader.readBytes(count);
    }

    @Override
    public boolean readBool() throws IOException {
        return reader.readBool();
    }

    @Override
    public String readString() throws IOException {
        final int count = reader.readVarUInt32();
        final byte[] bytes = reader.readBytes(count);
        return StringHelper.decodeString(bytes);
    }

    @Override
    public String readWString() throws IOException {
        final int count = reader.readVarUInt32();
        final byte[] bytes = reader.readBytes(count * 2);
        return StringHelper.decodeString(bytes);
    }

    @Override
    public void skip(BondDataType type) throws IOException {
        throw new RuntimeException("Not implemented.");
    }
}
