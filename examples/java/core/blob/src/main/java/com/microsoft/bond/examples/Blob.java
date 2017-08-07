package com.microsoft.bond.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import com.microsoft.bond.Deserializer;
import com.microsoft.bond.Serializer;
import com.microsoft.bond.StructBondType;

import com.microsoft.bond.protocol.CompactBinaryReader;
import com.microsoft.bond.protocol.CompactBinaryWriter;

import Examples.Example;

public class Blob {

    public static void main(final String[] args) throws IOException {

        final Example obj = new Example();
        // TO DO: Initialize fields

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Example> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Example> deserializer = new Deserializer<>((StructBondType<Example>) obj.getBondType());
        final Example obj2 = deserializer.deserialize(reader);

        assert obj.equals(obj2) : "Roundtrip failed";
    }

}
