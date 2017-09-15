// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;

/**
 * Implements Compact Binary serialization.
 * Refer to {@see https://microsoft.github.io/bond/reference/cpp/compact__binary_8h_source.html} for details.
 */
public final class CompactBinaryWriter implements TwoPassProtocolWriter {

    // Wraps a mutable integer that keeps track of the length of a struct.
    private static final class StructLength {
        int length = 0;
    }

    // Indicates the protocol type when marshalling.
    private static final ProtocolType MAGIC = ProtocolType.COMPACT_PROTOCOL;

    private final BinaryStreamWriter writer;
    private final short protocolVersion;
    private final LinkedList<StructLength> structLengths;

    public CompactBinaryWriter(final OutputStream outputStream, final int protocolVersion) {
        if (outputStream == null) {
            throw new IllegalArgumentException("Argument stream must not be null");
        }

        if (protocolVersion != 1 && protocolVersion != 2) {
            throw new IllegalArgumentException("Invalid protocol version: " + protocolVersion);
        }

        this.writer = new BinaryStreamWriter(outputStream);
        this.protocolVersion = (short) protocolVersion;

        this.structLengths = this.protocolVersion == 2 ? new LinkedList<StructLength>() : null;
    }

    @Override
    public boolean usesMarshaledBonded() {
        return false;
    }

    @Override
    public ProtocolWriter getFirstPassWriter() {
        return this.protocolVersion == 2 ? new FirstPassCounter() : null;
    }

    @Override
    public void writeVersion() throws IOException {
        this.writer.writeInt16((short) MAGIC.value);
        this.writer.writeInt16(this.protocolVersion);
    }

    @Override
    public void writeStructBegin(final Metadata metadata) throws IOException {
        if (this.protocolVersion == 2) {
            int structLength = this.structLengths.removeFirst().length;
            this.writer.writeVarUInt32(structLength);
        }
    }

    @Override
    public void writeStructEnd() throws IOException {
        this.writer.writeInt8((byte) BondDataType.BT_STOP.value);
    }

    @Override
    public void writeBaseBegin(final Metadata metadata) throws IOException {
    }

    @Override
    public void writeBaseEnd() throws IOException {
        this.writer.writeInt8((byte) BondDataType.BT_STOP_BASE.value);
    }

    @Override
    public void writeFieldBegin(
            final BondDataType type, final int id, final Metadata metadata) throws IOException {
        if (id <= 5) {
            this.writer.writeInt8((byte) (type.value | (id << 5)));
        } else if (id <= 0xFF) {
            this.writer.writeInt8((byte) (type.value | 0xC0));
            this.writer.writeInt8((byte) id);
        } else {
            this.writer.writeInt8((byte) (type.value | 0xE0));
            this.writer.writeInt16((short) id);
        }
    }

    @Override
    public void writeFieldEnd() throws IOException {
    }

    @Override
    public void writeFieldOmitted(
            final BondDataType type, final int id, final Metadata metadata) throws IOException {
    }

    @Override
    public void writeContainerBegin(final int count, final BondDataType elementType) throws IOException {
        if (this.protocolVersion == 2 && count < 7) {
            this.writer.writeInt8((byte) (elementType.value | ((count + 1) << 5)));
        } else {
            this.writer.writeInt8((byte) elementType.value);
            this.writer.writeVarUInt32(count);
        }
    }

    @Override
    public void writeContainerBegin(final int count, final BondDataType keyType, final BondDataType valueType)
            throws IOException {
        this.writer.writeInt8((byte) keyType.value);
        this.writer.writeInt8((byte) valueType.value);
        this.writer.writeVarUInt32(count);
    }

    @Override
    public void writeContainerEnd() throws IOException {
    }

    @Override
    public void writeInt8(final byte value) throws IOException {
        // reinterpret the sign bit as the high-order bit
        this.writer.writeInt8(value);
    }

    @Override
    public void writeInt16(final short value) throws IOException {
        // reinterpret the sign bit as the high-order bit
        this.writer.writeVarUInt16(ZigzagHelper.encodeZigzag16(value));
    }

    @Override
    public void writeInt32(final int value) throws IOException {
        // reinterpret the sign bit as the high-order bit
        this.writer.writeVarUInt32(ZigzagHelper.encodeZigzag32(value));
    }

    @Override
    public void writeInt64(final long value) throws IOException {
        // reinterpret the sign bit as the high-order bit
        this.writer.writeVarUInt64(ZigzagHelper.encodeZigzag64(value));
    }

    @Override
    public void writeUInt8(final byte value) throws IOException {
        this.writer.writeInt8(value);
    }

    @Override
    public void writeUInt16(final short value) throws IOException {
        this.writer.writeVarUInt16(value);
    }

    @Override
    public void writeUInt32(final int value) throws IOException {
        this.writer.writeVarUInt32(value);
    }

    @Override
    public void writeUInt64(final long value) throws IOException {
        this.writer.writeVarUInt64(value);
    }

    @Override
    public void writeFloat(final float value) throws IOException {
        this.writer.writeFloat(value);
    }

    @Override
    public void writeDouble(final double value) throws IOException {
        this.writer.writeDouble(value);
    }

    @Override
    public void writeBytes(final byte[] value) throws IOException {
        this.writer.writeBytes(value);
    }

    @Override
    public void writeBool(final boolean value) throws IOException {
        this.writer.writeInt8((byte) (value ? 1 : 0));
    }

    @Override
    public void writeString(final String value) throws IOException {
        if (value.isEmpty()) {
            // avoid calling encoder for an empty string
            this.writer.writeVarUInt32(0);
        } else {
            final byte[] bytes = StringHelper.encodeString(value);
            writer.writeVarUInt32(bytes.length);
            writer.writeBytes(bytes);
        }
    }

    @Override
    public void writeWString(final String value) throws IOException {
        if (value.isEmpty()) {
            // avoid calling encoder for an empty string
            this.writer.writeVarUInt32(0);
        } else {
            final byte[] bytes = StringHelper.encodeWString(value);
            writer.writeVarUInt32(bytes.length / 2);
            writer.writeBytes(bytes);
        }
    }

    // Implements the first-pass writer that collects lengths of all structs that need to be written.
    private final class FirstPassCounter implements ProtocolWriter {

        private final LinkedList<StructLength> workingStack = new LinkedList<StructLength>();

        @Override
        public boolean usesMarshaledBonded() {
            return false;
        }

        @Override
        public void writeVersion() throws IOException {
            // magic and version are not included in struct lengths
        }

        @Override
        public void writeStructBegin(final Metadata metadata) throws IOException {
            // start new stack frame for the struct
            StructLength stackFrame = new StructLength();
            this.workingStack.push(stackFrame);
            CompactBinaryWriter.this.structLengths.addLast(stackFrame);
        }

        @Override
        public void writeBaseBegin(final Metadata metadata) throws IOException {
        }

        @Override
        public void writeStructEnd() throws IOException {
            // complete the current struct
            this.addBytes(1); // BT_STOP
            final StructLength completedStackFrame = this.workingStack.pop();

            // update the enclosing struct (the new current)
            if (!this.workingStack.isEmpty()) {
                this.addBytes(VarUIntHelper.getVarUInt32Length(completedStackFrame.length));
                this.addBytes(completedStackFrame.length);
            }
        }

        @Override
        public void writeBaseEnd() throws IOException {
            this.addBytes(1); // BT_STOP_BASE
        }

        @Override
        public void writeFieldBegin(
                final BondDataType type, final int id, final Metadata metadata) throws IOException {
            if (id <= 5) {
                this.addBytes(1);
            } else if (id <= 0xFF) {
                this.addBytes(2);
            } else {
                this.addBytes(3);
            }
        }

        @Override
        public void writeFieldEnd() throws IOException {
        }

        @Override
        public void writeFieldOmitted(
                final BondDataType type, final int id, final Metadata metadata) throws IOException {
        }

        @Override
        public void writeContainerBegin(final int count, final BondDataType elementType)
                throws IOException {
            if (CompactBinaryWriter.this.protocolVersion == 2 && count < 7) {
                this.addBytes(1);
            } else {
                this.addBytes(1 + VarUIntHelper.getVarUInt32Length(count));
            }
        }

        @Override
        public void writeContainerBegin(final int count, final BondDataType keyType, final BondDataType valueType)
                throws IOException {
            this.addBytes(2 + VarUIntHelper.getVarUInt32Length(count));
        }

        @Override
        public void writeContainerEnd() throws IOException {
        }

        @Override
        public void writeInt8(final byte value) throws IOException {
            this.addBytes(1);
        }

        @Override
        public void writeInt16(final short value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt16Length(ZigzagHelper.encodeZigzag16(value)));
        }

        @Override
        public void writeInt32(final int value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt32Length(ZigzagHelper.encodeZigzag32(value)));
        }

        @Override
        public void writeInt64(final long value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt64Length(ZigzagHelper.encodeZigzag64(value)));
        }

        @Override
        public void writeUInt8(final byte value) throws IOException {
            this.addBytes(1);
        }

        @Override
        public void writeUInt16(final short value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt16Length(value));
        }

        @Override
        public void writeUInt32(final int value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt32Length(value));
        }

        @Override
        public void writeUInt64(final long value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt64Length(value));
        }

        @Override
        public void writeFloat(final float value) throws IOException {
            this.addBytes(4);
        }

        @Override
        public void writeDouble(final double value) throws IOException {
            this.addBytes(8);
        }

        @Override
        public void writeBytes(final byte[] value) throws IOException {
            this.addBytes(value.length);
        }

        @Override
        public void writeBool(final boolean value) throws IOException {
            this.addBytes(1);
        }

        @Override
        public void writeString(final String value) throws IOException {
            final int length = value.length();
            this.addBytes(VarUIntHelper.getVarUInt32Length(length));
            if (length > 0) {
                this.addBytes(StringHelper.getEncodedStringLength(value));
            }
        }

        @Override
        public void writeWString(final String value) throws IOException {
            final int length = value.length();
            this.addBytes(VarUIntHelper.getVarUInt32Length(length));
            if (length > 0) {
                this.addBytes(StringHelper.getEncodedWStringLength(value));
            }
        }

        private void addBytes(final int byteCount) {
            this.workingStack.peek().length += byteCount;
        }
    }
}
