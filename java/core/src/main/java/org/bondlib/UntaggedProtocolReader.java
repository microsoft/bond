// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.InputStream;

/**
 * Reads from serialized payload encoded using an untagged protocol.
 */
public interface UntaggedProtocolReader {

    /**
     * Start reading a container.
     */
    int readContainerBegin() throws IOException;

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
     * Skip an int8.
     */
    void skipInt8() throws IOException;

    /**
     * Read an int16.
     *
     * @return the value
     */
    short readInt16() throws IOException;

    /**
     * Skip an int16.
     */
    void skipInt16() throws IOException;

    /**
     * Read an int32.
     *
     * @return the value
     */
    int readInt32() throws IOException;

    /**
     * Skip an int32.
     */
    void skipInt32() throws IOException;

    /**
     * Read an int64.
     *
     * @return the value
     */
    long readInt64() throws IOException;

    /**
     * Skip an int64.
     */
    void skipInt64() throws IOException;

    /**
     * Read an uint8.
     *
     * @return the value
     */
    byte readUInt8() throws IOException;

    /**
     * Skip an uint8.
     */
    void skipUInt8() throws IOException;

    /**
     * Read an uint16.
     *
     * @return the value
     */
    short readUInt16() throws IOException;

    /**
     * Skip an uint16.
     */
    void skipUInt16() throws IOException;

    /**
     * Read an uint32.
     *
     * @return the value
     */
    int readUInt32() throws IOException;

    /**
     * Skip an uint32.
     */
    void skipUInt32() throws IOException;

    /**
     * Read an uint64.
     *
     * @return the value
     */
    long readUInt64() throws IOException;

    /**
     * Skip an uint64.
     */
    void skipUInt64() throws IOException;

    /**
     * Read a float.
     *
     * @return the value
     */
    float readFloat() throws IOException;

    /**
     * Skip a float.
     */
    void skipFloat() throws IOException;

    /**
     * Read a double.
     *
     * @return the value
     */
    double readDouble() throws IOException;

    /**
     * Skip a double.
     */
    void skipDouble() throws IOException;

    /**
     * Read an array of bytes verbatim.
     *
     * @param count
     * @return the value
     */
    byte[] readBytes(int count) throws IOException;

    /**
     * Skip forward a number of bytes.
     *
     * @param count number of bytes to skip
     */
    void skipBytes(int count) throws IOException;

    /**
     * Read a bool.
     *
     * @return the value
     */
    boolean readBool() throws IOException;

    /**
     * Skip a bool.
     */
    void skipBool() throws IOException;

    /**
     * Read a string.
     *
     * @return the value
     */
    String readString() throws IOException;

    /**
     * Skip a string.
     */
    void skipString() throws IOException;

    /**
     * Read a wstring.
     *
     * @return the value
     */
    String readWString() throws IOException;

    /**
     * Skip a wstring.
     */
    void skipWString() throws IOException;

    /**
     * Skip based on supplied runtime schema
     *
     * @param schema the schema def
     * @param typeDef the type def
     * @throws IOException
     */
    void skip(SchemaDef schema, TypeDef typeDef) throws IOException;

    /**
     * @return a clone of this reader's underlying stream
     */
    InputStream cloneStream() throws IOException;

    /**
     * Clones this untagged protocol reader.
     *
     * @return a clone of this stream
     * @throws IOException if there was an error cloning this stream
     */
    UntaggedProtocolReader cloneProtocolReader() throws IOException;
}
