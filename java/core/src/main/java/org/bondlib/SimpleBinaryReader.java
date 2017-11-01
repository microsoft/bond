// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.InputStream;

/**
 * Implements Simple Binary serialization.
 * Refer to {@see https://microsoft.github.io/bond/reference/cpp/simple__binary_8h_source.html} for details.
 */
public final class SimpleBinaryReader implements UntaggedProtocolReader {
    private final BinaryStreamReader reader;
    private final short protocolVersion;

    public SimpleBinaryReader(final InputStream inputStream, final int protocolVersion) {
        if (inputStream == null) {
            throw new IllegalArgumentException("Argument stream must not be null");
        }

        if (protocolVersion != 1 && protocolVersion != 2) {
            throw new IllegalArgumentException("Invalid protocol version: " + protocolVersion);
        }

        this.reader = new BinaryStreamReader(inputStream);
        this.protocolVersion = (short) protocolVersion;
    }

    private int readLength() throws IOException {
        if (this.protocolVersion == 2) {
            return this.reader.readVarUInt32();
        } else {
            return this.reader.readInt32();
        }
    }

    @Override
    public int readContainerBegin() throws IOException {
        return readLength();
    }

    @Override
    public void readContainerEnd() throws IOException {
    }

    @Override
    public byte readInt8() throws IOException {
        return this.reader.readInt8();
    }

    @Override
    public void skipInt8() throws IOException {
        this.reader.skipBytes(1);
    }

    @Override
    public short readInt16() throws IOException {
        return this.reader.readInt16();
    }

    @Override
    public void skipInt16() throws IOException {
        this.reader.skipBytes(2);
    }

    @Override
    public int readInt32() throws IOException {
        return this.reader.readInt32();
    }

    @Override
    public void skipInt32() throws IOException {
        this.reader.skipBytes(4);
    }

    @Override
    public long readInt64() throws IOException {
        return this.reader.readInt64();
    }

    @Override
    public void skipInt64() throws IOException {
        this.reader.skipBytes(8);
    }

    @Override
    public byte readUInt8() throws IOException {
        return this.reader.readInt8();
    }

    @Override
    public void skipUInt8() throws IOException {
        this.reader.skipBytes(1);
    }

    @Override
    public short readUInt16() throws IOException {
        return this.reader.readInt16();
    }

    @Override
    public void skipUInt16() throws IOException {
        this.reader.skipBytes(2);
    }

    @Override
    public int readUInt32() throws IOException {
        return this.reader.readInt32();
    }

    @Override
    public void skipUInt32() throws IOException {
        this.reader.skipBytes(4);
    }

    @Override
    public long readUInt64() throws IOException {
        return this.reader.readInt64();
    }

    @Override
    public void skipUInt64() throws IOException {
        this.reader.skipBytes(8);
    }

    @Override
    public float readFloat() throws IOException {
        return this.reader.readFloat();
    }

    @Override
    public void skipFloat() throws IOException {
        this.reader.skipBytes(4);
    }

    @Override
    public double readDouble() throws IOException {
        return this.reader.readDouble();
    }

    @Override
    public void skipDouble() throws IOException {
        this.reader.skipBytes(8);
    }

    @Override
    public byte[] readBytes(int count) throws IOException {
        return this.reader.readBytes(count);
    }

    @Override
    public void skipBytes(int count) throws IOException {
        this.reader.skipBytes(count);
    }

    @Override
    public boolean readBool() throws IOException {
        return this.reader.readBool();
    }

    @Override
    public void skipBool() throws IOException {
        this.reader.skipBytes(1);
    }

    @Override
    public String readString() throws IOException {
        final int codeUnitCount = readLength();
        if (codeUnitCount == 0) {
            // avoid calling decoder for an empty byte array
            return "";
        } else {
            final byte[] bytes = reader.readBytes(codeUnitCount);
            return StringHelper.decodeString(bytes);
        }
    }

    @Override
    public void skipString() throws IOException {
        final int codeUnitCount = readLength();
        this.reader.skipBytes(codeUnitCount);
    }

    @Override
    public String readWString() throws IOException {
        final int codeUnitCount = readLength();
        if (codeUnitCount == 0) {
            // avoid calling decoder for an empty byte array
            return "";
        } else {
            final byte[] bytes = reader.readBytes(codeUnitCount * 2);
            return StringHelper.decodeWString(bytes);
        }
    }

    @Override
    public void skipWString() throws IOException {
        final int codeUnitCount = readLength();
        this.reader.skipBytes(codeUnitCount * 2);
    }

    @Override
    public void skip(SchemaDef schemaDef, TypeDef type) throws IOException {
        switch (type.id.value) {

            case BondDataType.Values.BT_BOOL:
                this.skipBool();
                break;

            case BondDataType.Values.BT_UINT8:
                this.skipUInt8();
                break;

            case BondDataType.Values.BT_UINT16:
                this.skipUInt16();
                break;

            case BondDataType.Values.BT_UINT32:
                this.skipUInt32();
                break;

            case BondDataType.Values.BT_UINT64:
                this.skipUInt64();
                break;

            case BondDataType.Values.BT_FLOAT:
                this.skipFloat();
                break;

            case BondDataType.Values.BT_DOUBLE:
                this.skipDouble();
                break;

            case BondDataType.Values.BT_STRING:
                this.skipString();
                break;

            case BondDataType.Values.BT_STRUCT:
                if (type.bonded_type) {
                    int count = readLength();
                    skipBytes(count);
                } else {
                    final StructDef structDef = schemaDef.structs.get(type.struct_def);
                    final TypeDef baseDef = structDef.base_def;
                    if (baseDef != null) {
                        skip(schemaDef, baseDef);
                    }
                    for (final org.bondlib.FieldDef field : structDef.fields) {
                        skip(schemaDef, field.type);
                    }
                }
                break;

            case BondDataType.Values.BT_LIST:
            case BondDataType.Values.BT_SET: {
                int numElems = readLength();
                final TypeDef elementTypeDef = type.element;
                for (int i = 0; i < numElems; i++) {
                    skip(schemaDef, elementTypeDef);
                }
                break;
            }
            case BondDataType.Values.BT_MAP: {
                int numElems = readLength();
                final TypeDef keyTypeDef = type.key;
                final TypeDef valueTypeDef = type.element;
                for (int i = 0; i < numElems; i++) {
                    skip(schemaDef, keyTypeDef);
                    skip(schemaDef, valueTypeDef);
                }
                break;
            }
            case BondDataType.Values.BT_INT8:
                this.skipInt8();
                break;

            case BondDataType.Values.BT_INT16:
                this.skipInt16();
                break;

            case BondDataType.Values.BT_INT32:
                this.skipInt32();
                break;

            case BondDataType.Values.BT_INT64:
                this.skipInt64();
                break;

            case BondDataType.Values.BT_WSTRING:
                this.skipWString();
                break;

            default:
                throw new IllegalArgumentException("unknown BondDataType while skipping");
        }
    }

    @Override
    public InputStream cloneStream() throws IOException {
        return Cloning.cloneStream(this.reader.inputStream);
    }

    @Override
    public UntaggedProtocolReader cloneProtocolReader() throws IOException {
        InputStream clonedInputStream = Cloning.cloneStream(this.reader.inputStream);
        return new SimpleBinaryReader(clonedInputStream, this.protocolVersion);
    }
}

