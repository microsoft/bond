package org.bondlib.examples;

import org.bondlib.Deserializer;
import org.bondlib.SimpleBinaryReader;
import org.bondlib.SimpleBinaryWriter;
import org.bondlib.SchemaDef;
import org.bondlib.Serializer;

import org.bondlib.examples.untagged_protocols.Example1;
import org.bondlib.examples.untagged_protocols.Example2;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

// See build.gradle for namespace mapping

public class UntaggedProtocols {

    public static void main(final String[] args) throws IOException {

        final Example1 obj = new Example1();
        obj.Enabled = true;
        obj.Name = "foo";

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final SimpleBinaryWriter writer = new SimpleBinaryWriter(output, 1);

        final Serializer<Example1> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final SimpleBinaryReader reader = new SimpleBinaryReader(input, 1);
        final Deserializer<Example2> deserializer = new Deserializer<>(Example2.BOND_TYPE);

        final SchemaDef schema = Example1.BOND_TYPE.buildSchemaDef();
        final Example2 obj2 = deserializer.deserialize(reader, schema);

        assert obj.Enabled == obj2.Enabled;
        assert obj.Name.equals(obj2.Name);
    }

}
