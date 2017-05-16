package com.microsoft.bond.protocol;

import com.sun.xml.internal.ws.util.StreamUtils;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.util.Arrays;

import static org.junit.Assert.*;

public class StreamHelperTest {

    @Test
    public void callImplicitDefaultConstructor() {
        // adds the implicit default constructor to unit test code coverage
        new StreamHelper();
    }

    @Test
    public void readByte() {
        byte[] inputBytes = new byte[]{2, 7, 31, 127};

        // read bytes normally
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        try {
            byte b0 = StreamHelper.readByte(bais);
            assertEquals(2, b0);
            byte b1 = StreamHelper.readByte(bais);
            assertEquals(7, b1);
            byte b2 = StreamHelper.readByte(bais);
            assertEquals(31, b2);
            byte b3 = StreamHelper.readByte(bais);
            assertEquals(127, b3);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            byte b4 = StreamHelper.readByte(bais);
            fail("Operation can't succeed: " + b4);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readBytes() {
        byte[] inputBytes = new byte[]{2, 7, 31, 127};

        // iterate over output buffer offsets
        int maxOffset = 5;
        byte[] b = new byte[inputBytes.length + maxOffset];
        for (int offset = 0; offset < maxOffset; ++offset) {

            // iterate over length size
            for (int byteCount = 1; byteCount <= inputBytes.length; ++byteCount) {
                Arrays.fill(b, (byte) 0);

                // read bytes normally
                try {
                    ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
                    StreamHelper.readBytes(bais, b, offset, byteCount);

                    for (int i = 0; i < offset; ++i) {
                        assertEquals(0, b[i]);
                    }

                    for (int i = offset; i < offset + byteCount; ++i) {
                        assertEquals(inputBytes[i - offset], b[i]);
                    }

                    for (int i = offset + byteCount; i < b.length - 1; ++i) {
                        assertEquals(0, b[i]);
                    }
                } catch (IOException e) {
                    fail("IOException can't be thrown here: " + e);
                }

                // read past the end of stream
                try {
                    int eofByteCount = inputBytes.length + 1;
                    ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
                    StreamHelper.readBytes(bais, b, offset, eofByteCount);

                    fail("Operation can't succeed: " + eofByteCount);
                } catch (EOFException e) {
                    // success
                } catch (IOException e) {
                    fail("IOException can't be thrown here: " + e);
                }
            }
        }
    }
}