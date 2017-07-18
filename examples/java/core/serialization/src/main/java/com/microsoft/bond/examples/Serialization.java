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

import examples.serialization.Struct;

public class Serialization {

    private static final int STATUS_FAILURE = 255;

    public static void main(final String[] args) throws IOException {

        Struct obj = new Struct();
        obj.n = 0x1000;
        obj.str = "test";
        obj.items = new ArrayList<>(2);
        obj.items.add(3.14D);
        obj.items.add(0D);

        ByteArrayOutputStream output = new ByteArrayOutputStream();
        CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Struct> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Struct> deserializer = new Deserializer<>((StructBondType<Struct>) obj.getBondType());
        Struct obj2 = deserializer.deserialize(reader);

        System.exit(obj.equals(obj2) ? 0 : STATUS_FAILURE);
    }

}
