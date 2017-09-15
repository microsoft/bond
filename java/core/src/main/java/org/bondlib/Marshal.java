// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Resposible for marshaling of objects.
 */
public final class Marshal {

    // prevent instantiation
    private Marshal() {
    }

    /**
     * Marshals a struct object using the given protocol writer.
     *
     * @param obj       object to marshal
     * @param writer    protocol writer
     * @param <TStruct> the type of the object
     * @throws IOException if an I/O error occurred
     */
    public static <TStruct extends BondSerializable> void marshal(TStruct obj, ProtocolWriter writer)
            throws IOException {
        ArgumentHelper.ensureNotNull(obj, "obj");
        ArgumentHelper.ensureNotNull(writer, "writer");
        Serializer<TStruct> serializer = new Serializer<TStruct>();
        writer.writeVersion();
        serializer.serialize(obj, writer);
    }

    /**
     * Marshals a Bonded object using the given protocol writer.
     *
     * @param obj       Bonded object to marshal
     * @param writer    protocol writer
     * @param <TStruct> the type of the object
     * @throws IOException if an I/O error occurred
     */
    public static <TStruct extends BondSerializable> void marshal(Bonded<? extends TStruct> obj, ProtocolWriter writer)
            throws IOException {
        ArgumentHelper.ensureNotNull(obj, "obj");
        ArgumentHelper.ensureNotNull(writer, "writer");
        writer.writeVersion();
        obj.serialize(writer);
    }
}
