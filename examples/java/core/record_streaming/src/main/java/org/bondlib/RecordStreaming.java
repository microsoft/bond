package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;

import org.bondlib.Deserializer;
import org.bondlib.CompactBinaryReader;
import org.bondlib.CompactBinaryWriter;
import org.bondlib.Serializer;

import org.bondlib.examples.recordstreaming.Struct;

public class RecordStreaming {

    private static final int NUM_OBJECTS = 9;

    public static void main(final String[] args) throws IOException {

        // Create a stream of serialized structs.
        final ByteArrayOutputStream output = serializeStream();

        // Read them all back.
        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
        deserializeStream(input);
    }

    private static ByteArrayOutputStream serializeStream() throws IOException {
        // An output stream can be used to accumulate multiple serialized
        // structs. This can be done by re-using the same writer over the
        // same stream.
        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        // Use Compact Binary protocol for encoding; the data will be
        // written to output which will allocate more memory as needed.
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, (short)1);
        final Serializer<Struct> serializer = new Serializer<>();

        for (int n = 1; n <= NUM_OBJECTS; ++n) {
            final Struct obj = new Struct();
            obj.n = n;
            obj.str = String.join("", Collections.nCopies(n,"#"));
            serializer.serialize(obj, writer);
        }

        // At this point output contains the serialized representation of a
        // stream of objects. ByteArrayOutputStream.toByteArray() can be
        // used to get the entire buffer.
        return output;
    }

    private static void deserializeStream(ByteArrayInputStream input) throws IOException {
        final CompactBinaryReader reader = new CompactBinaryReader(input, (short)1);
        final Deserializer<Struct> deserializer = new Deserializer<>(Struct.BOND_TYPE);

        int numObjectsSeen = 0;

        // Each call to deserialize will advance the underlying stream's
        // position. When there are no more bytes available, all the structs
        // have been deserialized.
        //
        // You may find that you need to pack serialized Bond structs into a
        // stream in a different way depending on your needs. The reader
        // just needs its underlying stream to be positioned at the
        // beginning of a serialized struct.
        while (input.available() > 0) {
            final Struct obj = deserializer.deserialize(reader);
            ++numObjectsSeen;

            assert numObjectsSeen == obj.n;
            assert obj.n == obj.str.length();
        }

        assert numObjectsSeen == NUM_OBJECTS;
    }
}
