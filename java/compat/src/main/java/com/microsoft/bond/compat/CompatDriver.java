package com.microsoft.bond.compat;

import com.microsoft.bond.protocol.CompactBinaryReader;
import com.microsoft.bond.protocol.CompactBinaryWriter;
import com.microsoft.bond.protocol.FastBinaryReader;
import com.microsoft.bond.protocol.FastBinaryWriter;
import com.microsoft.bond.protocol.ProtocolWriter;
import com.microsoft.bond.protocol.TaggedProtocolReader;
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

        TaggedProtocolReader reader = null;
        ProtocolWriter writer = null;

        if (fromProtocol.equals("fast")) {
            reader = new FastBinaryReader(input, (short) 1);
        } else if (fromProtocol.equals("compact")) {
            reader = new CompactBinaryReader(input, (short) 1);
        } else if (fromProtocol.equals("compact2")) {
            reader = new CompactBinaryReader(input, (short) 2);
        } else {
            System.err.println("Unsupported input protocol: " + fromProtocol);
            System.exit(STATUS_FAILURE);
        }

        if (toProtocol.equals("fast")) {
            writer = new FastBinaryWriter(output, (short) 1);
        } else if (toProtocol.equals("compact")) {
            writer = new CompactBinaryWriter(output, (short) 1);
        } else if (toProtocol.equals("compact2")) {
            writer = new CompactBinaryWriter(output, (short) 2);
        } else {
            System.err.println("Unsupported output protocol: " + toProtocol);
            System.exit(STATUS_FAILURE);
        }

        final Compat compat = new Compat();
        compat.deserialize(reader);
        compat.serialize(writer);
    }
}
