// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Responsible for transcoding between protocols.
 */
public final class Transcoder {

    private final SchemaDef schema;

    public Transcoder(SchemaDef schema) {
        this.schema = schema;
    }

    public Transcoder() {
        this(null);
    }

    public final SchemaDef getSchema() {
        return this.schema;
    }

    /**
     * Transcodes from a tagged binary protocol to another protocol.
     *
     * @param fromProtocolReader source protocol reader
     * @param toProtocolWriter   target protocol writer
     * @throws UnsupportedProtocolException if transcoding is not supported
     * @throws IOException                  if an I/O error occurred
     */
    public void transcode(
            TaggedProtocolReader fromProtocolReader,
            ProtocolWriter toProtocolWriter) throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }

    /**
     * Transcodes from an untagged binary protocol to another protocol.
     *
     * @param fromProtocolReader source protocol reader
     * @param toProtocolWriter   target protocol writer
     * @throws UnsupportedProtocolException if transcoding is not supported
     * @throws IOException                  if an I/O error occurred
     */
    public void transcode(
            UntaggedProtocolReader fromProtocolReader,
            ProtocolWriter toProtocolWriter) throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }

    /**
     * Transcodes from a simple text protocol to another protocol.
     *
     * @param fromProtocolReader source protocol reader
     * @param toProtocolWriter   target protocol writer
     * @throws UnsupportedProtocolException if transcoding is not supported
     * @throws IOException                  if an I/O error occurred
     */
    public void transcode(
            TextProtocolReader fromProtocolReader,
            ProtocolWriter toProtocolWriter) throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }
}
