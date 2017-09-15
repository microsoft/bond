package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.Deserializer;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Serializer;

import org.bondlib.examples.serialization.Struct;

public class Serialization {

    public static void main(final String[] args) throws IOException {

        final Struct obj = new Struct();
        obj.n = 0x1000;
        obj.str = "test";
        obj.items.add(3.14D);
        obj.items.add(0D);

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<Struct> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
        final Deserializer<Struct> deserializer = new Deserializer<>(Struct.BOND_TYPE);
        final Struct obj2 = deserializer.deserialize(reader);

        assert obj.equals(obj2) : "Roundtrip failed";
    }

}
