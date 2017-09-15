package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Deserializer;
import org.bondlib.Serializer;

// See build.gradle for namespace mapping
import org.bondlib.examples.imports.Message;
import org.bondlib.examples.imports.common.Priority;

public class Imports {

    public static void main(final String[] args) throws IOException {

        final Message src = new Message();
        src.Header.Origin = "contoso.com";
        src.Header.Destination = "fabrikam.com";
        src.Priority = Priority.Normal;
        src.MessagePayload = 42;

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<Message> serializer = new Serializer<>();
        serializer.serialize(src, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
        final Deserializer<Message> deserializer = new Deserializer<>(Message.BOND_TYPE);
        final Message dst = deserializer.deserialize(reader);

        assert src.equals(dst) : "Roundtrip failed";
    }

}
