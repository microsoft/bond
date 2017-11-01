// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.test.Base;
import org.bondlib.test.HasBondedField;
import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class CompactBinaryWriterTest {

    // See CompactBinaryV1ProtocolTest and CompactBinaryV2ProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new CompactBinaryWriter(baos, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new CompactBinaryWriter(baos, 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullOutputStream() {
        ByteArrayOutputStream baos = null;
        new CompactBinaryWriter(baos, 1);
    }

    @Test
    public void testWriteFieldOmittedDoesNotWrite() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        CompactBinaryWriter writer = new CompactBinaryWriter(baos, 1);
        try {
            writer.writeFieldOmitted(null, 0, null);
            assertEquals(0, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void testWriteVersion() {
        int[] allowedVersions = new int[]{1, 2};
        for (int expectedVersion : allowedVersions) {
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
        CompactBinaryWriter w1 = new CompactBinaryWriter(baos, 1);
        assertNull(w1.getFirstPassWriter());
        CompactBinaryWriter w2 = new CompactBinaryWriter(baos, 2);
        assertNotNull(w2.getFirstPassWriter());
    }

    @Test
    public void testFirstWriterWriteVersionPassDoesNotWrite() {
        // write
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        CompactBinaryWriter writer = new CompactBinaryWriter(baos, 2);
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
        CompactBinaryWriter writer = new CompactBinaryWriter(baos, 2);
        ProtocolWriter firstPassWriter = writer.getFirstPassWriter();
        assertNotNull(firstPassWriter);
        try {
            firstPassWriter.writeFieldOmitted(null, 0, null);
            assertEquals(0, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }
    }

    /*
     * Regression test for a bug where there would be stale state after
     * serializing a Bonded field with a Compact v2 writer (the only stateful
     * writer). The bug was fixed in commit 8a608fbe.
     */
    @Test
    public void v2WReuseWithBonded() throws IOException {
        final int objCount = 3;

        final List<Base> basesIn = new ArrayList<Base>();
        for (int i = 0; i < objCount; i++) {
            final Base base = new Base();
            base.baseInt = i;
            basesIn.add(base);
        }

        final List<HasBondedField> structsIn = new ArrayList<HasBondedField>();
        for (int i = 0; i < objCount; i++) {
            final HasBondedField struct = new HasBondedField();
            struct.bondedField = Bonded.fromObject(basesIn.get(i), Base.BOND_TYPE);
            structsIn.add(struct);
        }

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(baos, 2);
        final Serializer<HasBondedField> serializer = new Serializer<HasBondedField>();
        for (int i = 0; i < objCount; i++) {
            serializer.serialize(structsIn.get(i), writer);
        }

        final ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        final CompactBinaryReader reader = new CompactBinaryReader(bais, 2);
        final Deserializer<HasBondedField> deserializer = new Deserializer<HasBondedField>(HasBondedField.BOND_TYPE);
        final List<Base> basesOut = new ArrayList<Base>();
        for (int i = 0; i < objCount; i++) {
            final HasBondedField struct = deserializer.deserialize(reader);
            basesOut.add(struct.bondedField.deserialize());
        }

        Assert.assertEquals(0, bais.available());
        Assert.assertEquals(basesIn, basesOut);
    }
}
