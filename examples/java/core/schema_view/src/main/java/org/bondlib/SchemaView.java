package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.Deserializer;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Serializer;

import org.bondlib.examples.schemaview.Example;
import org.bondlib.examples.schemaview.ExampleView;

public class SchemaView {

    public static void main(final String[] args) throws IOException {

        final Example example = new Example();
        example.num = 42;
        example.str = "test";
        example.items.add(3.14D);
        example.items.add(0D);

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<Example> serializer = new Serializer<>();
        serializer.serialize(example, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
        final Deserializer<ExampleView> deserializer = new Deserializer<>(ExampleView.BOND_TYPE);
        final ExampleView view = deserializer.deserialize(reader);

        assert example.num == view.num;
        assert example.str.equals(view.str);
    }
}
