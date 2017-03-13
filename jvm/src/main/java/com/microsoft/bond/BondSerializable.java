package com.microsoft.bond;

import com.microsoft.bond.protocol.ProtocolWriter;

import java.io.IOException;
import java.io.InputStream;


/**
 * Enables serialization of Bond classes.
 */
public interface BondSerializable {
    /**
     * Resets object to the default value.
     */
    //public void reset();

    /**
     * Deserializes Bond structure.
     * @param reader {@link ProtocolReader} with serialized data.
     * @throws IOException If there are any problems using the Protocol.
     */
    //void read(ProtocolReader reader) throws IOException;

    /**
     * Deserializes Bond structure.
     * @param reader {@link ProtocolReader} with serialized data.
     * @param schema {@link SchemaDef} describing data schema.
     * @throws IOException If there are any problems using the Protocol.
     */
    //void read(ProtocolReader reader, BondSerializable schema) throws IOException;

    /**
     * Deserializes Bond structure as a nested field, element or base.
     * @param reader {@link ProtocolReader} with serialized data.
     * @param schema {@link SchemaDef} describing data schema.
     * @throws IOException If there are any problems using the Protocol.
     */
    //void readNested(ProtocolReader reader) throws java.io.IOException;

    /**
     * Unmarshals Bond structure.
     * @param input {@link InputStream} with serialized data.
     * @throws IOException If there are any problems using the stream.
     */
    //void unmarshal(InputStream input) throws IOException;

    /**
     * Unmarshals Bond structure.
     * @param input {@link InputStream} with serialized data.
     * @param schema {@link SchemaDef} describing data schema.
     * @throws IOException If there are any problems using the stream.
     */
    //void unmarshal(InputStream input, BondSerializable schema) throws IOException;

    /**
     * Serializes Bond structure.
     * @param writer {@link ProtocolWriter} for serialized data.
     * @throws IOException If there are any problems using the Protocol.
     */
    void write(ProtocolWriter writer) throws IOException;

    /**
     * Serializes Bond structure as a nested field, value or base.
     * @param writer {@link ProtocolWriter} for serialized data.
     * @param isBase True if serializing as a base schema
     * @throws IOException If there are any problems using the Protocol.
     */
    //void writeNested(ProtocolWriter writer, boolean isBase) throws java.io.IOException;

    /**
     * Marshals Bond structure.
     * @param writer {@link ProtocolWriter} for serialized data.
     * @throws IOException If there are any problems using the Protocol.
     */
    //void marshal(ProtocolWriter writer) throws IOException;

    /**
     * Performs field-by-field comparison of two objects.
     *
     * Works like a bitwise compare, notice the following edge cases:
     *
     * <ul>
     * <li>Two null fields considered equal to each other.</li>
     * <li>Two empty collections are equal to each other.</li>
     * <li>An empty collection is not equal to a null collection.</li>
     * <li>A float/double field of NaN considered equal to other NaN.</li>
     * </ul>
     */
    //boolean memberwiseCompare(Object that);

    /**
     * Performs deep cloning.
     * @return cloned object.
     */
    //BondSerializable clone();
}
