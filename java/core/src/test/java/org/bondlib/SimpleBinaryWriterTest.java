// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class SimpleBinaryWriterTest {

    // See SimpleBinaryProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new SimpleBinaryWriter(baos, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new SimpleBinaryWriter(baos, 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullOutputStream() {
        new SimpleBinaryWriter(null, 1);
    }

    @Test
    public void testWriteVersion() {
        int[] allowedVersions = new int[] { 1, 2 };
        for (int expectedVersion : allowedVersions) {
            // write
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            SimpleBinaryWriter w = new SimpleBinaryWriter(baos, expectedVersion);
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
                assertEquals(ProtocolType.SIMPLE_PROTOCOL.value, magic);
                short actualVersion = bsr.readInt16();
                assertEquals(expectedVersion, actualVersion);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }
        }
    }
}