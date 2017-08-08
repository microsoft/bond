package com.microsoft.bond.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import com.microsoft.bond.Deserializer;
import com.microsoft.bond.Serializer;

import com.microsoft.bond.protocol.CompactBinaryReader;
import com.microsoft.bond.protocol.CompactBinaryWriter;

import com.microsoft.bond.examples.imports.Message;
import com.microsoft.bond.examples.imports.common.Priority;

public class Imports {

    public static void main(final String[] args) throws IOException {

        final Message src = new Message();
        src.Header.Origin = "contoso.com";
        src.Header.Destination = "fabrikam.com";
        src.Priority = Priority.Normal;
        src.MessagePayload = 42;

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short) 1);

        final Serializer<Message> serializer = new Serializer<>();
        serializer.serialize(src, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, (short) 1);
        final Deserializer<Message> deserializer = new Deserializer<>(Message.BOND_TYPE);
        final Message dst = deserializer.deserialize(reader);

        assert src.equals(dst) : "Roundtrip failed";
    }

}
