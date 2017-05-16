package com.microsoft.bond.protocol;

import com.microsoft.bond.BondDataType;

/**
 * Reads from serialized payload encoded using a tagged protocol.
 */
public interface TaggedProtocolReader {

    /**
     * Contains the result of reading a field of a struct.
     */
    final class ReadFieldResult {

        /**
         * Set to the field type or BT_STOP/BT_STOP_BASE if there is no more fields in current struct/base.
         */
        public BondDataType type;

        /**
         * Set to the field identifier.
         */
        public int id;
    }

    /**
     * Contains the result of reading a container (either list/set or a map).
     */
    final class ReadContainerResult {

        /**
         * Set to number of items in the container.
         */
        public int count;

        /**
         * Set to type of container elements (for list/set) or the type map values (for map).
         */
        public BondDataType elementType;

        /**
         * Set to the type of map keys (for map). Unused for list/set containers.
         */
        public BondDataType keyType;
    }

    /**
     * Start reading a struct.
     */
    void readStructBegin();

    /**
     * Start reading a base of a struct.
     */
    void readBaseBegin();

    /**
     * End reading a struct.
     */
    void readStructEnd();

    /**
     * End reading a base of a struct.
     */
    void readBaseEnd();

    /**
     * Start reading a field.
     * @param result contains the result
     */
    void readFieldBegin(ReadFieldResult result);

    /**
     * End reading a field.
     */
    void readFieldEnd();

    /**
     * Start reading a list or set container.
     * @param readContainerResult contains the result
     */
    void readListBegin(ReadContainerResult readContainerResult);

    /**
     * Start reading a map container.
     * @param readContainerResult contains the result
     */
    void readMapBegin(ReadContainerResult readContainerResult);

    /**
     * End reading a container.
     */
    void readContainerEnd();

    /**
     * Skip a value of specified type.
     * @param type the type
     */
    void skip(BondDataType type);

    /**
     * Read an int8.
     * @return the value
     */
    byte readInt8();

    /**
     * Read an int16.
     * @return the value
     */
    short readInt16();

    /**
     * Read an int32.
     * @return the value
     */
    int readInt32();

    /**
     * Read an int64.
     * @return the value
     */
    long readInt64();

    /**
     * Read an uint8.
     * @return the value
     */
    byte readUInt8();

    /**
     * Read an uint16.
     * @return the value
     */
    short readUInt16();

    /**
     * Read an uint32.
     * @return the value
     */
    int readUInt32();

    /**
     * Read an uint64.
     * @return the value
     */
    long readUInt64();

    /**
     * Read a float.
     * @return the value
     */
    float readFloat();

    /**
     * Read a double.
     * @return the value
     */
    double readDouble();

    /**
     * Read an array of bytes verbatim.
     * @return the value
     */
    byte[] readBytes();

    /**
     * Read a bool.
     * @return the value
     */
    boolean readBool();

    /**
     * Read a string.
     * @return the value
     */
    String readString();

    /**
     * Read a wstring.
     * @return the value
     */
    String readWString();
}
