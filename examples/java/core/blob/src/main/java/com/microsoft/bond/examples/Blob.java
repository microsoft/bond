package com.microsoft.bond.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;

import com.microsoft.bond.Deserializer;
import com.microsoft.bond.Serializer;
import com.microsoft.bond.Something;

import com.microsoft.bond.protocol.CompactBinaryReader;
import com.microsoft.bond.protocol.CompactBinaryWriter;

// See build.gradle for namespace mapping
import com.microsoft.bond.examples.blob.Example;

public class Blob {

    public static void main(final String[] args) throws IOException {

        final byte[] data = new byte[256];
        for (int i = 0; i < data.length; i++) {
            data[i] = (byte) i;
        }

        final Example obj = new Example();
        obj.ListOfBlobs.add(Arrays.copyOfRange(data, 0, 10));
        obj.ListOfBlobs.add(Arrays.copyOfRange(data, 10, 20));
        obj.NullableBlob = Arrays.copyOfRange(data, 20, 30);
        obj.UninitializeBlob = Something.wrap(Arrays.copyOfRange(data, 30, 100));

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Example> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Example> deserializer = new Deserializer<>(Example.BOND_TYPE);
        final Example obj2 = deserializer.deserialize(reader);

        assert obj.equals(obj2) : "Roundtrip failed";
    }

}
