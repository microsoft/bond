package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;

import org.bondlib.Blob;
import org.bondlib.Deserializer;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Serializer;
import org.bondlib.Something;

// See build.gradle for namespace mapping
import org.bondlib.examples.blob.Example;

public class BlobExample {

    public static void main(final String[] args) throws IOException {

        final byte[] data = new byte[256];
        for (int i = 0; i < data.length; i++) {
            data[i] = (byte) i;
        }

        final Example obj = new Example();
        obj.ListOfBlobs.add(new Blob(Arrays.copyOfRange(data, 0, 10)));
        obj.ListOfBlobs.add(new Blob(Arrays.copyOfRange(data, 10, 20)));
        obj.NullableBlob = new Blob(Arrays.copyOfRange(data, 20, 30));
        obj.UninitializedBlob = Something.wrap(new Blob(Arrays.copyOfRange(data, 30, 100)));

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<Example> serializer = new Serializer<>();
        serializer.serialize(obj, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
        final Deserializer<Example> deserializer = new Deserializer<>(Example.BOND_TYPE);
        final Example obj2 = deserializer.deserialize(reader);

        assert obj.equals(obj2) : "Roundtrip failed";
    }

}
