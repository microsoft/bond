// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Reads from serialized payload encoded using a structured text protocol such as JSON or XML.
 */
public interface TextProtocolReader {

    /**
     * Start reading a struct. The method returns struct name or null if struct name is not supported
     * by the implementing protocol.
     *
     * @return struct name if supported by the protool or null otherwise
     */
    String readStructBegin() throws IOException;

    /**
     * End reading a struct.
     */
    void readStructEnd() throws IOException;

    /**
     * Start reading a field. The method returns field name or null if the end of current struct
     * has been reached.
     *
     * @return field name if can read a field or null if the end of struct has been reached
     */
    String readFieldBegin() throws IOException;

    /**
     * End reading a field.
     */
    void readFieldEnd() throws IOException;

    /**
     * Start reading a collection container.
     */
    void readContainerBegin() throws IOException;

    /**
     * End reading a collection container.
     */
    void readContainerEnd() throws IOException;

    /**
     * Start reading a collection container item. The method returns true is the collection has
     * items to read or returns false otherwise.
     *
     * @return true if the container item was read or false if the end of collection has been reached
     */
    boolean readContainerItemBegin() throws IOException;

    /**
     * End reading a collection container item.
     */
    void readContainerItemEnd() throws IOException;

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
    TextProtocolReader cloneProtocolReader() throws IOException;
}
