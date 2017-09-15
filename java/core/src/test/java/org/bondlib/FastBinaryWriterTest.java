// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class FastBinaryWriterTest {

    // See FastBinaryProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new FastBinaryWriter(baos, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new FastBinaryWriter(baos, 2);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullOutputStream() {
        ByteArrayOutputStream baos = null;
        new FastBinaryWriter(baos, 1);
    }

    @Test
    public void testWriteFieldOmittedDoesNotWrite() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        FastBinaryWriter writer = new FastBinaryWriter(baos, 1);
        try {
            writer.writeFieldOmitted(null, 0, null);
            assertEquals(0, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void testWriteVersion() {
        int[] allowedVersions = new int[] { 1 };
        for (int expectedVersion : allowedVersions) {
            // write
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            FastBinaryWriter w = new FastBinaryWriter(baos, expectedVersion);
            try {
                w.writeVersion();
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }

            // verify
            ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
            BinaryStreamReader bsr = new BinaryStreamReader(bais);
            try {
                short magic = bsr.readInt16();
                assertEquals(ProtocolType.FAST_PROTOCOL.value, magic);
                short actualVersion = bsr.readInt16();
                assertEquals(expectedVersion, actualVersion);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }
        }
    }
}