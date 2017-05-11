package com.microsoft.bond.protocol;

import com.microsoft.bond.BondDataType;
import com.microsoft.bond.Metadata;
import com.microsoft.bond.ProtocolType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

// TODO: add Compact Binary protocol format comment (either here and/or in the deserializer and/or in package.html)

/**
 * Implements Compact Binary protocol (serialization).
 */
public final class CompactBinaryProtocolWriter implements TwoPassProtocolWriter {

    private static final short MAGIC = (short)ProtocolType.COMPACT_PROTOCOL.value;

    private final BinaryStreamWriter writer;
    private final short version;
    private final FirstPassCounter firstPassCounter;

    private int currentStructIndex;

    public CompactBinaryProtocolWriter(OutputStream outputStream, short version) {
        this.writer = new BinaryStreamWriter(outputStream);
        this.version = version;
        this.firstPassCounter = (version == 2) ? new FirstPassCounter() : null;
        this.currentStructIndex = 0;
    }

    @Override
    public void writeVersion() throws IOException {
        this.writer.writeInt16(MAGIC);
        this.writer.writeInt16(this.version);
    }

    @Override
    public void writeStructBegin(Metadata metadata) throws IOException {
        if (this.version == 2) {
            int structLength = this.firstPassCounter.getResultLength(this.currentStructIndex);
            this.writer.writeVarUInt32(structLength);
        }

        ++this.currentStructIndex;
    }

    @Override
    public void writeBaseBegin(Metadata metadata) throws IOException {}

    @Override
    public void writeStructEnd() throws IOException {
        this.writer.writeInt8((byte)BondDataType.BT_STOP.value);

        --this.currentStructIndex;
    }

    @Override
    public void writeBaseEnd() throws IOException {
        this.writer.writeInt8((byte)BondDataType.BT_STOP_BASE.value);
    }

    @Override
    public void writeFieldBegin(BondDataType type, int id, Metadata metadata) throws IOException {
        if (id <= 5) {
            this.writer.writeInt8((byte)(type.value | (id << 5)));
        }
        else if (id <= 0xFF) {
            this.writer.writeInt8((byte)(type.value | 0xC0));
            this.writer.writeInt8((byte)id);
        }
        else {
            this.writer.writeInt8((byte)(type.value | 0xE0));
            this.writer.writeInt16((short)id);
        }
    }

    @Override
    public void writeFieldEnd() throws IOException {}

    @Override
    public void writeFieldOmitted(BondDataType type, int id, Metadata metadata) throws IOException {}

    @Override
    public void writeContainerBegin(int count, BondDataType elementType) throws IOException {
        if (this.version == 2 && count < 7) {
            this.writer.writeInt8((byte)(elementType.value | ((count + 1) << 5)));
        }
        else {
            this.writer.writeInt8((byte)elementType.value);
            this.writer.writeVarUInt32(count);
        }
    }

    @Override
    public void writeContainerBegin(int count, BondDataType keyType, BondDataType valueType) throws IOException {
        this.writer.writeInt8((byte)keyType.value);
        this.writer.writeInt8((byte)valueType.value);
        this.writer.writeVarUInt32(count);
    }

    @Override
    public void writeContainerEnd() throws IOException {}

    @Override
    public void writeInt8(byte value) throws IOException {
        this.writer.writeInt8(value);
    }

    @Override
    public void writeInt16(short value) throws IOException {
        this.writer.writeVarUInt16(ZigzagHelper.encodeZigzag16(value));
    }

    @Override
    public void writeInt32(int value) throws IOException {
        this.writer.writeVarUInt32(ZigzagHelper.encodeZigzag32(value));
    }

    @Override
    public void writeInt64(long value) throws IOException {
        this.writer.writeVarUInt64(ZigzagHelper.encodeZigzag64(value));
    }

    @Override
    public void writeUInt8(byte value) throws IOException {
        this.writer.writeInt8(value);
    }

    @Override
    public void writeUInt16(short value) throws IOException {
        this.writer.writeVarUInt16(value);
    }

    @Override
    public void writeUInt32(int value) throws IOException {
        this.writer.writeVarUInt32(value);
    }

    @Override
    public void writeUInt64(long value) throws IOException {
        this.writer.writeVarUInt64(value);
    }

    @Override
    public void writeFloat(float value) throws IOException {
        this.writer.writeFloat(value);
    }

    @Override
    public void writeDouble(double value) throws IOException {
        this.writer.writeDouble(value);
    }

    @Override
    public void writeBytes(byte[] value) throws IOException {
        this.writer.writeBytes(value);
    }

    @Override
    public void writeBool(boolean value) throws IOException {
        this.writer.writeInt8((byte)(value ? 1 : 0));
    }

    @Override
    public void writeString(String value) throws IOException {
        if (value.isEmpty()) {
            // avoid calling encoder for an empty string
            this.writer.writeInt32(0);
        }
        else {
            byte[] bytes = StringHelper.encodeString(value);
            this.writer.writeVarUInt32(bytes.length);
            this.writer.writeBytes(bytes);
        }
    }

    @Override
    public void writeWString(String value) throws IOException {
        if (value.isEmpty()) {
            // avoid calling encoder for an empty string
            this.writer.writeVarUInt32(0);
        }
        else {
            byte[] bytes = StringHelper.encodeWString(value);
            this.writer.writeVarUInt32(bytes.length / 2);
            this.writer.writeBytes(bytes);
        }
    }

    @Override
    public ProtocolWriter getFirstPassWriter() {
        return this.firstPassCounter;
    }

    /**
     * Implements the first-pass writer that collects lengths of all structs that need to be written.
     */
    private final class FirstPassCounter implements ProtocolWriter {

        /**
         * Wraps a mutable integer that keeps track of the length of the current struct.
         */
        private final class StructLength {
            int length = 0;
        }

        /**
         * Extends ArrayList by adding stack functionality. Doesn't check for emptiness when popping.
         */
        private final class ArrayListStack<T> extends ArrayList<T> {
            void push(T e) {
                this.add(e);
            }

            T pop() {
                return this.remove(this.size() - 1);
            }

            T peek() {
                return this.get(this.size() - 1);
            }
        }

        private final ArrayListStack<StructLength> workingStack = new ArrayListStack<StructLength>();
        private final ArrayList<StructLength> resultLengths = new ArrayList<StructLength>();

        @Override
        public void writeVersion() throws IOException {}

        @Override
        public void writeStructBegin(Metadata metadata) throws IOException {
            // start new stack frame for the struct
            this.workingStack.push(new StructLength());
        }

        @Override
        public void writeBaseBegin(Metadata metadata) throws IOException {}

        @Override
        public void writeStructEnd() throws IOException {
            // complete the current struct
            StructLength completedStackFrame = this.workingStack.pop();
            completedStackFrame.length += 1; // BT_STOP
            this.resultLengths.add(completedStackFrame);

            // update the enclosing struct (the new current)
            if (!this.workingStack.isEmpty()) {
                this.addBytes(VarUIntHelper.getVarUInt32Length(completedStackFrame.length) + completedStackFrame.length);
            }
        }

        @Override
        public void writeBaseEnd() throws IOException {
            this.addBytes(1); // BT_STOP_BASE
        }

        @Override
        public void writeFieldBegin(BondDataType type, int id, Metadata metadata) throws IOException {
            if (id <= 5) {
                this.addBytes(1);
            }
            else if (id <= 0xFF) {
                this.addBytes(2);
            }
            else {
                this.addBytes(3);
            }
        }

        @Override
        public void writeFieldEnd() throws IOException {}

        @Override
        public void writeFieldOmitted(BondDataType type, int id, Metadata metadata) throws IOException {}

        @Override
        public void writeContainerBegin(int count, BondDataType elementType) throws IOException {
            if (CompactBinaryProtocolWriter.this.version == 2 && count < 7) {
                this.addBytes(1);
            }
            else {
                this.addBytes(1 + VarUIntHelper.getVarUInt32Length(count));
            }
        }

        @Override
        public void writeContainerBegin(int count, BondDataType keyType, BondDataType valueType) throws IOException {
            this.addBytes(2 + VarUIntHelper.getVarUInt32Length(count));
        }

        @Override
        public void writeContainerEnd() throws IOException {}

        @Override
        public void writeInt8(byte value) throws IOException {
            this.addBytes(1);
        }

        @Override
        public void writeInt16(short value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt16Length(ZigzagHelper.encodeZigzag16(value)));
        }

        @Override
        public void writeInt32(int value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt32Length(ZigzagHelper.encodeZigzag32(value)));
        }

        @Override
        public void writeInt64(long value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt64Length(ZigzagHelper.encodeZigzag64(value)));
        }

        @Override
        public void writeUInt8(byte value) throws IOException {
            this.addBytes(1);
        }

        @Override
        public void writeUInt16(short value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt16Length(value));
        }

        @Override
        public void writeUInt32(int value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt32Length(value));
        }

        @Override
        public void writeUInt64(long value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt64Length(value));
        }

        @Override
        public void writeFloat(float value) throws IOException {
            this.addBytes(4);
        }

        @Override
        public void writeDouble(double value) throws IOException {
            this.addBytes(8);
        }

        @Override
        public void writeBytes(byte[] value) throws IOException {
            this.addBytes(value.length);
        }

        @Override
        public void writeBool(boolean value) throws IOException {
            this.addBytes(1);
        }

        @Override
        public void writeString(String value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt32Length(value.length()));
            this.addBytes(StringHelper.getEncodedStringLength(value));
        }

        @Override
        public void writeWString(String value) throws IOException {
            this.addBytes(VarUIntHelper.getVarUInt32Length(value.length()));
            this.addBytes(StringHelper.getEncodedWStringLength(value) / 2);
        }

        // called by the enclosing class to get results given the index of struct in depth-first traversal
        int getResultLength(int index) {
            return this.resultLengths.get(index).length;
        }

        private void addBytes(int byteCount) {
            this.workingStack.peek().length += byteCount;
        }
    }
}
