// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.InputStream;

/**
 * Implements Compact Binary deserialization.
 * Refer to {@see https://microsoft.github.io/bond/reference/cpp/compact__binary_8h_source.html} for details.
 */
public final class CompactBinaryReader implements TaggedProtocolReader {

    private final BinaryStreamReader reader;
    private final short protocolVersion;

    public CompactBinaryReader(final InputStream inputStream, final int protocolVersion) {
        if (inputStream == null) {
            throw new IllegalArgumentException("Argument stream must not be null");
        }

        if (protocolVersion != 1 && protocolVersion != 2) {
            throw new IllegalArgumentException("Invalid protocol version: " + protocolVersion);
        }

        this.reader = new BinaryStreamReader(inputStream);
        this.protocolVersion = (short) protocolVersion;
    }

    private BondDataType readType() throws IOException {
        return BondDataType.get(this.reader.readInt8());
    }

    @Override
    public void readStructBegin() throws IOException {
        if (this.protocolVersion == 2) {
            this.reader.readVarUInt32();
        }
    }

    @Override
    public void readStructEnd() throws IOException {
    }

    @Override
    public void readBaseBegin() throws IOException {
    }

    @Override
    public void readBaseEnd() throws IOException {
    }

    @Override
    public void readFieldBegin(final ReadFieldResult result) throws IOException {
        final int raw = UnsignedHelper.asUnsignedInt(this.reader.readInt8());
        final int embeddedId = raw >>> 5;

        int id;
        if (embeddedId == 6) {
            id = UnsignedHelper.asUnsignedInt(this.reader.readInt8());
        } else if (embeddedId == 7) {
            id = UnsignedHelper.asUnsignedInt(this.reader.readInt16());
        } else {
            id = embeddedId;
        }

        result.id = id;
        result.type = BondDataType.get(raw & 0x1F);
    }

    @Override
    public void readFieldEnd() throws IOException {
    }

    @Override
    public void readListBegin(ReadContainerResult result) throws IOException {
        final int raw = UnsignedHelper.asUnsignedInt(this.reader.readInt8());

        result.keyType = null;
        result.elementType = BondDataType.get(raw & 0x1F);

        if (this.protocolVersion == 2 && (raw & 0xE0) != 0) {
            result.count = (raw >>> 5) - 1;
        } else {
            result.count = this.reader.readVarUInt32();
        }
    }

    @Override
    public void readMapBegin(ReadContainerResult result) throws IOException {
        result.keyType = this.readType();
        result.elementType = this.readType();
        result.count = this.reader.readVarUInt32();
    }

    @Override
    public void readContainerEnd() throws IOException {
    }

    @Override
    public byte readInt8() throws IOException {
        return this.reader.readInt8();
    }

    @Override
    public short readInt16() throws IOException {
        return ZigzagHelper.decodeZigzag16(this.reader.readVarUInt16());
    }

    @Override
    public int readInt32() throws IOException {
        return ZigzagHelper.decodeZigzag32(this.reader.readVarUInt32());
    }

    @Override
    public long readInt64() throws IOException {
        return ZigzagHelper.decodeZigzag64(this.reader.readVarUInt64());
    }

    @Override
    public byte readUInt8() throws IOException {
        return this.reader.readInt8();
    }

    @Override
    public short readUInt16() throws IOException {
        return this.reader.readVarUInt16();
    }

    @Override
    public int readUInt32() throws IOException {
        return this.reader.readVarUInt32();
    }

    @Override
    public long readUInt64() throws IOException {
        return this.reader.readVarUInt64();
    }

    @Override
    public float readFloat() throws IOException {
        return this.reader.readFloat();
    }

    @Override
    public double readDouble() throws IOException {
        return this.reader.readDouble();
    }

    @Override
    public byte[] readBytes(int count) throws IOException {
        return this.reader.readBytes(count);
    }

    @Override
    public boolean readBool() throws IOException {
        return this.reader.readBool();
    }

    @Override
    public String readString() throws IOException {
        final int codeUnitCount = reader.readVarUInt32();
        if (codeUnitCount == 0) {
            // avoid calling decoder for an empty byte array
            return "";
        } else {
            final byte[] bytes = reader.readBytes(codeUnitCount);
            return StringHelper.decodeString(bytes);
        }
    }

    @Override
    public String readWString() throws IOException {
        final int codeUnitCount = reader.readVarUInt32();
        if (codeUnitCount == 0) {
            // avoid calling decoder for an empty byte array
            return "";
        } else {
            final byte[] bytes = reader.readBytes(codeUnitCount * 2);
            return StringHelper.decodeWString(bytes);
        }
    }

    @Override
    public void skip(final BondDataType type) throws IOException {
        switch (type.value) {
            // 1-byte
            case BondDataType.Values.BT_BOOL:
            case BondDataType.Values.BT_UINT8:
            case BondDataType.Values.BT_INT8:
                this.reader.skipBytes(1);
                break;

            // 4-byte
            case BondDataType.Values.BT_FLOAT:
                this.reader.skipBytes(4);
                break;

            // 8-byte
            case BondDataType.Values.BT_DOUBLE:
                this.reader.skipBytes(8);
                break;

            // variable-length encoding
            case BondDataType.Values.BT_INT16:
            case BondDataType.Values.BT_UINT16:
                this.reader.readVarUInt16();
                break;

            // variable-length encoding
            case BondDataType.Values.BT_INT32:
            case BondDataType.Values.BT_UINT32:
                this.reader.readVarUInt32();
                break;

            // variable-length encoding
            case BondDataType.Values.BT_INT64:
            case BondDataType.Values.BT_UINT64:
                this.reader.readVarUInt64();
                break;

            // UTF-8 string (1-byte Unicode code units)
            case BondDataType.Values.BT_STRING:
                this.reader.skipBytes(this.reader.readVarUInt32());
                break;

            // UTF-16 string (2-byte Unicode code units)
            case BondDataType.Values.BT_WSTRING:
                this.reader.skipBytes(this.reader.readVarUInt32() * 2L);
                break;

            // List/Set collections (recursive)
            case BondDataType.Values.BT_LIST:
            case BondDataType.Values.BT_SET:
                this.skipListContainer();
                break;

            // Map collections (recursive)
            case BondDataType.Values.BT_MAP:
                this.skipMapContainer();
                break;

            // Nested struct (recursive)
            case BondDataType.Values.BT_STRUCT:
                this.skipStruct();
                break;

            // Invalid type
            default:
                throw new InvalidBondDataException("Invalid Bond data type: " + type);
        }
    }

    private void skipListContainer() throws IOException {
        final int raw = UnsignedHelper.asUnsignedInt(this.reader.readInt8());
        final BondDataType elementType = BondDataType.get(raw & 0x1F);

        int count;
        if (this.protocolVersion == 2 && (raw & 0xE0) != 0) {
            count = (raw >>> 5) - 1;
        } else {
            count = this.reader.readVarUInt32();
        }

        // Process fixed-width data types separately to avoid looping over all elements
        int elementTypeFixedWidth = getFixedTypeWidth(elementType);
        if (elementTypeFixedWidth > 0) {
            this.reader.skipBytes((long) count * elementTypeFixedWidth);
        } else {
            while (--count >= 0) {
                this.skip(elementType);
            }
        }
    }

    private void skipMapContainer() throws IOException {
        final BondDataType keyType = this.readType();
        final BondDataType elementType = this.readType();
        int count = this.reader.readVarUInt32();

        // Process fixed-width data types separately to avoid looping over all elements
        int keyTypeFixedWidth = getFixedTypeWidth(elementType);
        int elementTypeFixedWidth = getFixedTypeWidth(elementType);
        if (keyTypeFixedWidth > 0 && elementTypeFixedWidth > 0) {
            this.reader.skipBytes((long) count * (keyTypeFixedWidth + elementTypeFixedWidth));
        } else {
            while (--count >= 0) {
                this.skip(keyType);
                this.skip(elementType);
            }
        }
    }

    private void skipStruct() throws IOException {
        if (this.protocolVersion == 2) {
            // take advantage of the struct length stored in V2
            this.reader.skipBytes(this.reader.readVarUInt32());
        } else {
            while (true) {
                final int raw = UnsignedHelper.asUnsignedInt(this.reader.readInt8());
                BondDataType fieldType = BondDataType.get(raw & 0x1F);
                final int embeddedId = raw >>> 5;
                if (embeddedId == 6) {
                    this.reader.skipBytes(1);
                } else if (embeddedId == 7) {
                    this.reader.skipBytes(2);
                }

                if (fieldType.value == BondDataType.BT_STOP_BASE.value) {
                    // don't stop, as there may be more fields following the base struct
                    continue;
                }

                if (fieldType.value == BondDataType.BT_STOP.value) {
                    // stop, as we've reached then end and there are no more fields
                    break;
                }

                this.skip(fieldType);
            }
        }
    }

    private static int getFixedTypeWidth(final BondDataType type) {
        switch (type.value) {
            // 1-byte
            case BondDataType.Values.BT_BOOL:
            case BondDataType.Values.BT_UINT8:
            case BondDataType.Values.BT_INT8:
                return 1;

            // 4-byte
            case BondDataType.Values.BT_FLOAT:
                return 4;

            // 8-byte
            case BondDataType.Values.BT_DOUBLE:
                return 8;

            // Not a fixed-width type
            default:
                return 0;
        }
    }

    @Override
    public TaggedProtocolReader cloneProtocolReader() throws IOException {
        InputStream clonedInputStream = Cloning.cloneStream(this.reader.inputStream);
        return new CompactBinaryReader(clonedInputStream, this.protocolVersion);
    }
}
