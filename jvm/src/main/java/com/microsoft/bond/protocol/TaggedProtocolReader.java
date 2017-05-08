package com.microsoft.bond.protocol;

import com.microsoft.bond.BondDataType;

/**
 * Reads a serialized payload.
 */
public interface TaggedProtocolReader {
    final class ReadFieldResult {
        public BondDataType type;
        public int id;
    }

    final class ReadContainerResult {
        public long count;
        public BondDataType element;
        public BondDataType key;
    }

    void readStructBegin();

    void readBaseBegin();

    void readStructEnd();

    void readBaseEnd();

    void readFieldBegin(ReadFieldResult result);

    void readFieldEnd();

    /**
     * Start reading a list or set container.
     */
    void readListBegin(ReadContainerResult readContainerResult);

    /**
     * Start reading a map container.
     */
    void readMapBegin(ReadContainerResult readContainerResult);

    void readContainerEnd();

    byte readInt8();

    short readInt16();

    int readInt32();

    long readInt64();

    byte readUInt8();

    short readUInt16();

    int readUInt32();

    long readUInt64();

    float readFloat();

    double readDouble();

    byte[] readBytes();

    boolean readBool();

    String readString();

    String readWString();

    void skip(BondDataType type);
}
