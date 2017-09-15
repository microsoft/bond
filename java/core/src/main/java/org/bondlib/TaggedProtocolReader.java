// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

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
    void readStructBegin() throws IOException;

    /**
     * End reading a struct.
     */
    void readStructEnd() throws IOException;

    /**
     * Start reading a base of a struct.
     */
    void readBaseBegin() throws IOException;

    /**
     * End reading a base of a struct.
     */
    void readBaseEnd() throws IOException;

    /**
     * Start reading a field.
     *
     * @param result contains the result
     */
    void readFieldBegin(ReadFieldResult result) throws IOException;

    /**
     * End reading a field.
     */
    void readFieldEnd() throws IOException;

    /**
     * Start reading a list or set container.
     *
     * @param result contains the result
     */
    void readListBegin(ReadContainerResult result) throws IOException;

    /**
     * Start reading a map container.
     *
     * @param result contains the result
     */
    void readMapBegin(ReadContainerResult result) throws IOException;

    /**
     * End reading a container.
     */
    void readContainerEnd() throws IOException;

    /**
     * Read an int8.
     *
     * @return the value
     */
    byte readInt8() throws IOException;

    /**
     * Read an int16.
     *
     * @return the value
     */
    short readInt16() throws IOException;

    /**
     * Read an int32.
     *
     * @return the value
     */
    int readInt32() throws IOException;

    /**
     * Read an int64.
     *
     * @return the value
     */
    long readInt64() throws IOException;

    /**
     * Read an uint8.
     *
     * @return the value
     */
    byte readUInt8() throws IOException;

    /**
     * Read an uint16.
     *
     * @return the value
     */
    short readUInt16() throws IOException;

    /**
     * Read an uint32.
     *
     * @return the value
     */
    int readUInt32() throws IOException;

    /**
     * Read an uint64.
     *
     * @return the value
     */
    long readUInt64() throws IOException;

    /**
     * Read a float.
     *
     * @return the value
     */
    float readFloat() throws IOException;

    /**
     * Read a double.
     *
     * @return the value
     */
    double readDouble() throws IOException;

    /**
     * Read an array of bytes verbatim.
     *
     * @param count
     * @return the value
     */
    byte[] readBytes(int count) throws IOException;

    /**
     * Read a bool.
     *
     * @return the value
     */
    boolean readBool() throws IOException;

    /**
     * Read a string.
     *
     * @return the value
     */
    String readString() throws IOException;

    /**
     * Read a wstring.
     *
     * @return the value
     */
    String readWString() throws IOException;

    /**
     * Skip a value of specified type.
     *
     * @param type the type
     */
    void skip(BondDataType type) throws IOException;

    /**
     * Clones this tagged protocol reader.
     *
     * @return a clone of this stream
     * @throws IOException if there was an error cloning this stream
     */
    TaggedProtocolReader cloneProtocolReader() throws IOException;
}
