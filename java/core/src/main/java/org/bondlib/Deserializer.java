// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Responsible for deserialization of objects of a particular struct.
 * @param <TStruct> the class of the deserialized struct value
 */
public final class Deserializer<TStruct extends BondSerializable> {

    private final StructBondType<TStruct> structType;

    /**
     * Initializes a new deserializer.
     *
     * @param structType the type descriptor of the deserialized struct
     */
    public Deserializer(StructBondType<TStruct> structType) {
        ArgumentHelper.ensureNotNull(structType, "structType");
        this.structType = structType;
    }

    /**
     * Deserializes an object from the given tagged protocol reader.
     *
     * @param reader the protocol reader to read from
     * @return deserialized object
     * @throws IOException if an I/O error occurred
     */
    public final TStruct deserialize(TaggedProtocolReader reader) throws IOException {
        ArgumentHelper.ensureNotNull(reader, "reader");
        return this.structType.deserialize(reader);
    }

    /**
     * Deserializes an object from the given untagged protocol reader.
     *
     * @param reader the protocol reader to read from
     * @return deserialized object
     * @throws IOException if an I/O error occurred
     */
    public final TStruct deserialize(UntaggedProtocolReader reader) throws IOException {
        ArgumentHelper.ensureNotNull(reader, "reader");
        return this.structType.deserialize(reader);
    }

    /**
     * Deserializes an object from the given untagged protocol reader using the given schema

     * @param reader the protocol reader to read from
     * @param schema the runtime schema
     * @return deserialized object
     * @throws IOException if an I/O error occurred
     */
    public final TStruct deserialize(UntaggedProtocolReader reader, SchemaDef schema) throws IOException {
        ArgumentHelper.ensureNotNull(reader, "reader");
        ArgumentHelper.ensureNotNull(schema, "schema");
        return this.structType.deserialize(reader, schema);
    }
}
