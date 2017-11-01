// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Resposible for serialization of objects of a particular struct.
 * @param <TStruct> the class of the serialized struct value
 */
public final class Serializer<TStruct extends BondSerializable> {

    /**
     * Serializes an object into the given protocol writer.
     *
     * @param obj    the object to serialize
     * @param writer the protocol writer to write into
     * @throws IOException if an I/O error occurred
     */
    public final void serialize(TStruct obj, ProtocolWriter writer) throws IOException {
        ArgumentHelper.ensureNotNull(obj, "obj");
        ArgumentHelper.ensureNotNull(writer, "writer");
        @SuppressWarnings("unchecked")
        StructBondType<TStruct> struct = (StructBondType<TStruct>) obj.getBondType();
        struct.serialize(obj, writer);
    }
}
