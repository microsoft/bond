// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

/**
 * Resposible for unmarshaling of objects.
 */
public final class Unmarshal {

    /**
     * Unmarshal payload into a Bonded instance.
     *
     * @param inputStream input stream
     * @param bondType    bond type of unmarshalled struct
     * @param <TStruct>   the type of unmarshalled struct
     * @return a bonded instance
     * @throws IOException if an I/O error occurred
     */
    public static <TStruct extends BondSerializable> Bonded<TStruct> unmarshal(
            InputStream inputStream, StructBondType<TStruct> bondType) throws IOException {
        ArgumentHelper.ensureNotNull(inputStream, "inputStream");

        short protocol = readInt16(inputStream);
        short version = readInt16(inputStream);

        switch (protocol) {
            case ProtocolType.Values.COMPACT_PROTOCOL:
                return Bonded.fromProtocolReader(new CompactBinaryReader(inputStream, version), bondType);

            case ProtocolType.Values.FAST_PROTOCOL:
                return Bonded.fromProtocolReader(new FastBinaryReader(inputStream, version), bondType);

            case ProtocolType.Values.SIMPLE_PROTOCOL:
                return Bonded.fromProtocolReader(new SimpleBinaryReader(inputStream, version), bondType);

            default:
                throw new InvalidBondDataException("Unknown protocol type: " + protocol);
        }
    }

    private static short readInt16(InputStream inputStream) throws IOException {
        // Bond protocol/version is little-endian
        int l = inputStream.read();
        if (l == -1) {
            throw new EOFException();
        }
        int h = inputStream.read();
        if (h == -1) {
            throw new EOFException();
        }
        return (short) ((h << 8) | l);
    }
}
