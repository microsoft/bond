// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.protocol;

import com.microsoft.bond.ProtocolType;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.*;

public class CompactBinaryWriterTest {

    // See CompactBinaryV1ProtocolTest and CompactBinaryV2ProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new CompactBinaryWriter(baos, (short) 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new CompactBinaryWriter(baos, (short) 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullOutputStream() {
        ByteArrayOutputStream baos = null;
        new CompactBinaryWriter(baos, (short) 1);
    }

    @Test
    public void testWriteFieldOmittedDoesNotWrite() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        CompactBinaryWriter writer = new CompactBinaryWriter(baos, (short) 1);
        try {
            writer.writeFieldOmitted(null, 0, null);
            assertEquals(0, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void testWriteVersion() {
        short[] allowedVersions = new short[]{1, 2};
        for (short expectedVersion : allowedVersions) {
            // write
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            CompactBinaryWriter w = new CompactBinaryWriter(baos, expectedVersion);
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
                assertEquals(ProtocolType.COMPACT_PROTOCOL.value, magic);
                short actualVersion = bsr.readInt16();
                assertEquals(expectedVersion, actualVersion);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }
        }
    }

    @Test
    public void testFirstPassWriterExistsOnlyForV2() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        CompactBinaryWriter w1 = new CompactBinaryWriter(baos, (short) 1);
        assertNull(w1.getFirstPassWriter());
        CompactBinaryWriter w2 = new CompactBinaryWriter(baos, (short) 2);
        assertNotNull(w2.getFirstPassWriter());
    }

    @Test
    public void testFirstWriterWriteVersionPassDoesNotWrite() {
        // write
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        CompactBinaryWriter writer = new CompactBinaryWriter(baos, (short) 2);
        ProtocolWriter firstPassWriter = writer.getFirstPassWriter();
        assertNotNull(firstPassWriter);
        try {
            firstPassWriter.writeVersion();
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        assertEquals(0, baos.size());
    }

    @Test
    public void testFirstPassWriterWriteFieldOmittedDoesNotWrite() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        CompactBinaryWriter writer = new CompactBinaryWriter(baos, (short) 2);
        ProtocolWriter firstPassWriter = writer.getFirstPassWriter();
        assertNotNull(firstPassWriter);
        try {
            firstPassWriter.writeFieldOmitted(null, 0, null);
            assertEquals(0, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }
    }
}
