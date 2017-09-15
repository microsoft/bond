package org.bondlib.compat;

import org.bondlib.*;
import unittest.compat.Compat;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

public class CompatDriver {
    private static final int STATUS_FAILURE = 255;

    public static void main(final String[] args) throws IOException {
        if (args.length < 3 || args.length > 4) {
            System.out.println("Usage:");
            System.out.println("CompatDriver json|compact|compact2|fast|simple|simple2|schema input_file output_file [json|compact|fast|simple|simple2]");
            System.exit(STATUS_FAILURE);
        }

        final String fromProtocol = args[0];
        final String inputFile = args[1];
        final String outputFile = args[2];
        final String toProtocol;
        if (args.length == 4) {
            toProtocol = args[3];
        } else {
            toProtocol = fromProtocol;
        }

        final FileInputStream input = new FileInputStream(inputFile);
        final FileOutputStream output = new FileOutputStream(outputFile);

        TaggedProtocolReader taggedReader = null;
        UntaggedProtocolReader untaggedReader = null;
        ProtocolWriter writer = null;

        switch (fromProtocol) {
            case "fast":
                taggedReader = new FastBinaryReader(input, (short) 1);
                break;
            case "compact":
                taggedReader = new CompactBinaryReader(input, (short) 1);
                break;
            case "compact2":
                taggedReader = new CompactBinaryReader(input, (short) 2);
                break;
            case "simple":
                untaggedReader = new SimpleBinaryReader(input, (short) 1);
                break;
            case "simple2":
                untaggedReader = new SimpleBinaryReader(input, (short) 2);
                break;
            default:
                System.err.println("Unsupported input protocol: " + fromProtocol);
                System.exit(STATUS_FAILURE);
        }

        switch (toProtocol) {
            case "fast":
                writer = new FastBinaryWriter(output, (short) 1);
                break;
            case "compact":
                writer = new CompactBinaryWriter(output, (short) 1);
                break;
            case "compact2":
                writer = new CompactBinaryWriter(output, (short) 2);
                break;
            case "simple":
                writer = new SimpleBinaryWriter(output, (short) 1);
                break;
            case "simple2":
                writer = new SimpleBinaryWriter(output, (short) 2);
                break;
            default:
                System.err.println("Unsupported output protocol: " + toProtocol);
                System.exit(STATUS_FAILURE);
        }

        final Serializer<Compat> serializer = new Serializer<>();
        final Deserializer<Compat> deserializer = new Deserializer<>(Compat.BOND_TYPE);
        final Compat compat;
        if (untaggedReader != null) {
            compat = deserializer.deserialize(untaggedReader);
        } else {
            compat = deserializer.deserialize(taggedReader);
        }
        serializer.serialize(compat, writer);
    }
}
