// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.BondSerializable;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Deserializer;
import org.bondlib.Serializer;

import org.bondlib.examples.inheritance.Base;
import org.bondlib.examples.inheritance.Derived;

public class Inheritance {

    public static void main(final String[] args) throws IOException {

        final Derived derived = new Derived();
        derived.str = "derived";
        ((Base)derived).str = "derived base";
        final CompactBinaryReader derivedPayload = new CompactBinaryReader(
            new ByteArrayInputStream(serialize(derived)),
            1);

        // Deserialize Base from payload containing Derived
        final Deserializer<Base> deserializer = new Deserializer<>(Base.BOND_TYPE);
        final Base obj = deserializer.deserialize(derivedPayload);
        assert obj.equals(derived): "Base parts should be equal";
    }

    private static <T extends BondSerializable> byte[] serialize(T obj) throws IOException {
        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<T> serializer = new Serializer<>();
        serializer.serialize(obj, writer);
        return output.toByteArray();
    }
}
