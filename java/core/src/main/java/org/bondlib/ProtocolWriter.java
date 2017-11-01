// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.BondDataType;
import org.bondlib.Metadata;

import java.io.IOException;

/**
 * Writes a serialized payload.
 */
public interface ProtocolWriter {

    /**
     * @return true if bonded fields in this protocol are marshaled; false if
     * they are serialized in the same protocol
     */
    boolean usesMarshaledBonded();

    /**
     * Write protocol magic number and version.
     */
    void writeVersion() throws IOException;

    /**
     * Start writing a struct.
     *
     * @param metadata struct metadata
     */
    void writeStructBegin(Metadata metadata) throws IOException;

    /**
     * End writing a struct.
     */
    void writeStructEnd() throws IOException;

    /**
     * Start writing a base struct.
     *
     * @param metadata base struct metadata
     */
    void writeBaseBegin(Metadata metadata) throws IOException;

    /**
     * End writing a base struct.
     */
    void writeBaseEnd() throws IOException;

    /**
     * Start writing a field.
     *
     * @param type     type of the field
     * @param id       identifier of the field
     * @param metadata metadata of the field
     */
    void writeFieldBegin(BondDataType type, int id, Metadata metadata) throws IOException;

    /**
     * End writing a field.
     */
    void writeFieldEnd() throws IOException;

    /**
     * Indicate that field was omitted because it was set to its default value.
     *
     * @param type     type of the field
     * @param id       identifier of the field
     * @param metadata metadata of the field
     */
    void writeFieldOmitted(BondDataType type, int id, Metadata metadata) throws IOException;

    /**
     * Start writing a list or set container.
     *
     * @param count       number of elements in the container
     * @param elementType type of the elements
     */
    void writeContainerBegin(int count, BondDataType elementType) throws IOException;

    /**
     * Start writing a map container
     *
     * @param count     number of elements in the container
     * @param keyType   type of the keys
     * @param valueType type of the values
     */
    void writeContainerBegin(int count, BondDataType keyType, BondDataType valueType) throws IOException;

    /**
     * End writing a container.
     */
    void writeContainerEnd() throws IOException;

    /**
     * Write an int8.
     *
     * @param value the value
     */
    void writeInt8(byte value) throws IOException;

    /**
     * Write an int16.
     *
     * @param value the value
     */
    void writeInt16(short value) throws IOException;

    /**
     * Write an int32.
     *
     * @param value the value
     */
    void writeInt32(int value) throws IOException;

    /**
     * Write an int64.
     *
     * @param value the value
     */
    void writeInt64(long value) throws IOException;

    /**
     * Write an uint8.
     *
     * @param value the value
     */
    void writeUInt8(byte value) throws IOException;

    /**
     * Write an uint16.
     *
     * @param value the value
     */
    void writeUInt16(short value) throws IOException;

    /**
     * Write an uint32.
     *
     * @param value the value
     */
    void writeUInt32(int value) throws IOException;

    /**
     * Write an uint64.
     *
     * @param value the value
     */
    void writeUInt64(long value) throws IOException;

    /**
     * Write a float.
     *
     * @param value the value
     */
    void writeFloat(float value) throws IOException;

    /**
     * Write a double.
     *
     * @param value the value
     */
    void writeDouble(double value) throws IOException;

    /**
     * Write an array of bytes verbatim.
     *
     * @param value the value
     */
    void writeBytes(byte[] value) throws IOException;

    /**
     * Write a bool.
     *
     * @param value the value
     */
    void writeBool(boolean value) throws IOException;

    /**
     * Write a string.
     *
     * @param value the value
     */
    void writeString(String value) throws IOException;

    /**
     * Write a wstring.
     *
     * @param value the value
     */
    void writeWString(String value) throws IOException;
}
