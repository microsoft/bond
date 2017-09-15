// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

/**
 * Contains helper methods for working with input and output streams.
 */
final class StreamHelper {

    // prevent instantiation
    private StreamHelper() {
    }

    /**
     * Reads a single byte from the input, raising an EOFException if end of stream is reached.
     *
     * @param inputStream source input stream
     * @return a byte
     * @throws EOFException if end of stream reached
     * @throws IOException  if other I/O error occurred
     */
    static byte readByte(InputStream inputStream) throws IOException {
        int b = inputStream.read();
        if (b < 0) {
            throw new EOFException();
        }

        return (byte) b;
    }

    /**
     * Reads a sequence of bytes from the input, raising an EOFException if end of stream is reached.
     *
     * @param inputStream source input stream
     * @param buffer      destination buffer
     * @param offset      offset in the buffer
     * @param length      number of bytes to read
     * @throws EOFException if end of stream reached
     * @throws IOException  if other I/O error occurred
     */
    static void readBytes(InputStream inputStream, byte[] buffer, int offset, int length) throws IOException {
        if (inputStream.read(buffer, offset, length) < length) {
            throw new EOFException();
        }
    }

    /**
     * Skips a sequence of bytes from the input, raising an EOFException if end of stream is reached.
     *
     * @param inputStream source input stream
     * @param length      number of bytes to skip
     * @throws EOFException if end of stream reached
     * @throws IOException  if other I/O error occurred
     */
    static void skipBytes(InputStream inputStream, long length) throws IOException {
        long actualSkippedLength = inputStream.skip(length);
        if (actualSkippedLength < length) {
            // assume reached the end of file (although the API mentions "other possibilities")
            throw new EOFException();
        }
    }
}
