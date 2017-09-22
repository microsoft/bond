// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Responsible for transcoding between protocols.
 */
public final class Transcoder {

    // prevent instantiation
    private Transcoder() {
    }

    /**
     * Transcodes from a tagged binary protocol to another protocol.
     *
     * @param fromProtocolReader source protocol reader
     * @param toProtocolWriter   target protocol writer
     * @param schema             optional schema which may be required for transcoding between some protocols
     * @throws UnsupportedOperationException if the schema is null and the transcoding requires it
     * @throws IOException                   if an I/O error occurred
     */
    public void transcode(
            TaggedProtocolReader fromProtocolReader,
            ProtocolWriter toProtocolWriter,
            SchemaDef schema) throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }

    /**
     * Transcodes from an untagged binary protocol to another protocol.
     *
     * @param fromProtocolReader source protocol reader
     * @param toProtocolWriter   target protocol writer
     * @param schema             optional schema which may be required for transcoding between some protocols
     * @throws UnsupportedOperationException if the schema is null and the transcoding requires it
     * @throws IOException                   if an I/O error occurred
     */
    public void transcode(
            UntaggedProtocolReader fromProtocolReader,
            ProtocolWriter toProtocolWriter,
            SchemaDef schema) throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }

    /**
     * Transcodes from a simple text protocol to another protocol.
     *
     * @param fromProtocolReader source protocol reader
     * @param toProtocolWriter   target protocol writer
     * @param schema             optional schema which may be required for transcoding between some protocols
     * @throws UnsupportedOperationException if the schema is null and the transcoding requires it
     * @throws IOException                   if an I/O error occurred
     */
    public void transcode(
            TextProtocolReader fromProtocolReader,
            ProtocolWriter toProtocolWriter,
            SchemaDef schema) throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }
}
