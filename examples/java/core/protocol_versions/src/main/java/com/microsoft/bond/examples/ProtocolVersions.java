package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.Deserializer;
import org.bondlib.Serializer;

import org.bondlib.protocol.CompactBinaryReader;
import org.bondlib.protocol.CompactBinaryWriter;

import org.bondlib.examples.protocolversions.Struct;

public class ProtocolVersions {

    public static void main(final String[] args) throws IOException {

        final Struct obj = new Struct();
        obj.n = 0x1000;
        obj.str = "test";
        obj.items.add(3.14D);
        obj.items.add(0D);

        final Serializer<Struct> serializer = new Serializer<>();
        final Deserializer<Struct> deserializer = new Deserializer<>(Struct.BOND_TYPE);

        // Protocols may have different versions with different features.
        // When serializing/deserializing the same version needs to be used.
        //
        // Marshaling can be used to embed the protocol and version in the
        // payload so the reading side can automatically determine which
        // protocol and version to use.
        {
            // Here, we use CompactBinary v1.
            final ByteArrayOutputStream output = new ByteArrayOutputStream();
            final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);
            serializer.serialize(obj, writer);

            final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
            final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
            final Struct obj2 = deserializer.deserialize(reader);

            assert obj.equals(obj2) : "Roundtrip CBv1 failed";
        }

        {
            // Here, we use CompactBinary v2.
            final ByteArrayOutputStream output = new ByteArrayOutputStream();
            final CompactBinaryWriter writer = new CompactBinaryWriter(output, 2);
            serializer.serialize(obj, writer);

            final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
            final CompactBinaryReader reader = new CompactBinaryReader(input, 2);
            final Struct obj2 = deserializer.deserialize(reader);

            assert obj.equals(obj2) : "Roundtrip CBv2 failed";
        }

        assert false : "Add marshaling example here";
    }
}
