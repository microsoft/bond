// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import com.microsoft.bond.BondSerializable;
import com.microsoft.bond.Deserializer;
import com.microsoft.bond.Serializer;

import com.microsoft.bond.protocol.CompactBinaryReader;
import com.microsoft.bond.protocol.CompactBinaryWriter;

import com.microsoft.bond.examples.inheritance.Base;
import com.microsoft.bond.examples.inheritance.Derived;

public class Inheritance {

    public static void main(final String[] args) throws IOException {

        final Derived derived = new Derived();
        derived.str = "derived";
        ((Base)derived).str = "derived base";
        final CompactBinaryReader derivedPayload = new CompactBinaryReader(
            new ByteArrayInputStream(serialize(derived)),
            (short) 1);

        // Deserialize Base from payload containing Derived
        final Deserializer<Base> deserializer = new Deserializer<>(Base.BOND_TYPE);
        final Base obj = deserializer.deserialize(derivedPayload);
        assert obj.equals(derived): "Base parts should be equal";
    }

    private static <T extends BondSerializable> byte[] serialize(T obj) throws IOException {
        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<T> serializer = new Serializer<>();
        serializer.serialize(obj, writer);
        return output.toByteArray();
    }
}
