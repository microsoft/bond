package com.microsoft.bond.protocol;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;

/**
 * Contains helper methods for working with input and output streams.
 */
final class StreamHelper {

    /**
     * Reads a single byte from the input, raising an EOFException if end of stream is reached.
     * @param inputStream source input stream
     * @return a byte
     * @throws EOFException if end of stream reached
     * @throws IOException if other I/O error occurred
     */
    static byte readByte(InputStream inputStream) throws IOException {
        int b = inputStream.read();
        if (b < 0) {
            // EOFException is self-explanatory and doesn't need a message
            throw new EOFException();
        }

        return (byte)b;
    }

    /**
     * Reads a sequence of bytes from the input, raising an EOFException if end of stream is reached.
     * @param inputStream source input stream
     * @param buffer destination buffer
     * @param offset offset in the buffer
     * @param length number of bytes to read
     * @throws EOFException if end of stream reached
     * @throws IOException if other I/O error occurred
     */
    static void readBytesIntoBuffer(InputStream inputStream, byte[] buffer, int offset, int length) throws IOException {
        if (inputStream.read(buffer, offset, length) < length) {
            // EOFException is self-explanatory and doesn't need a message
            throw new EOFException();
        }
    }
}
