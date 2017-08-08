package com.microsoft.bond.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import com.microsoft.bond.Deserializer;
import com.microsoft.bond.Serializer;

import com.microsoft.bond.protocol.CompactBinaryReader;
import com.microsoft.bond.protocol.CompactBinaryWriter;

import com.microsoft.bond.examples.serialization.Struct;

public class Serialization {

    public static void main(final String[] args) throws IOException {

        final Struct obj = new Struct();
        obj.n = 0x1000;
        obj.str = "test";
        obj.items.add(3.14D);
        obj.items.add(0D);

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Struct> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Struct> deserializer = new Deserializer<>(Struct.BOND_TYPE);
        final Struct obj2 = deserializer.deserialize(reader);

        assert obj.equals(obj2) : "Roundtrip failed";
    }

}
