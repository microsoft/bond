package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.LinkedList;

import org.bondlib.BondSerializable;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Deserializer;
import org.bondlib.Serializer;
import org.bondlib.Something;
import org.bondlib.StructBondType;

import org.bondlib.examples.nothingdefault.Struct_v1;
import org.bondlib.examples.nothingdefault.Struct_v2;

public class NothingDefault {

    public static void main(final String[] args) throws IOException {

        final Struct_v1 v1 = new Struct_v1();

        boolean sawException = false;

        // Struct_v1 has a required field foo which by default is set to 'nothing'.
        // If we try to serialize object v1 w/o initializing the field to some value
        // Bond will throw an exception.
        try {
            System.out.println("Serializing v1...");
            serialize(v1);
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
            sawException = true;
        }

        assert sawException : "Expected serialization to throw, as required nothing field is missing";

        // Initialize field by assigning a Something value to it...
        v1.foo = Something.wrap(10);

        // ... or for complex fields, use Something.getValue() to get a
        // reference to the thing.
        v1.baz = Something.wrap(new LinkedList<String>());
        v1.baz.getValue().add("test1");
        v1.baz.getValue().add("test2");

        // We can also set a field to 'nothing' by assigning null to the
        // field itself. Optional fields that are set to 'nothing' are
        // omitted when object is serialized.
        v1.baz = null;

        final byte[] buffer = serialize(v1);

        // Deserialize the payload into object of type Struct_v2
        final Struct_v2 v2 = deserialize(buffer, Struct_v2.BOND_TYPE);

        // Struct_v2 has an optional field bar, which didn't exist in
        // Struct_v1. It is initialized to 'nothing' by default. By checking
        // if the field is 'nothing' (== null) after de-serialization we can
        // detect if it was present in the payload or not.
        assert v2.baz == null : "baz was expected to have been omitted because it was set to nothing";
        assert v2.bar == null : "v1 didn't have a bar field, so it should be nothing in v2";
    }

    private static <T extends BondSerializable> byte[] serialize(T obj) throws IOException {
        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<T> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        return output.toByteArray();
    }

    private static <T extends BondSerializable> T deserialize(
        byte[] buffer,
        StructBondType<T> bondType) throws IOException {

        final ByteArrayInputStream input = new ByteArrayInputStream(buffer);
        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
        final Deserializer<T> deserializer = new Deserializer<>(bondType);
        return deserializer.deserialize(reader);
    }
}
