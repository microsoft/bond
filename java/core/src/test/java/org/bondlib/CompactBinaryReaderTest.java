// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import static org.junit.Assert.fail;

public class CompactBinaryReaderTest {

    // See CompactBinaryV1ProtocolTest and CompactBinaryV2ProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        new CompactBinaryReader(bais, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        new CompactBinaryReader(bais, 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullInputStream() {
        ByteArrayInputStream bais = null;
        new CompactBinaryReader(bais, 1);
    }

    @Test
    public void testSkipWithInvalidDataType() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        CompactBinaryReader reader = new CompactBinaryReader(bais, 1);
        BondDataType dataType = BondDataType.BT_UNAVAILABLE;
        try {
            reader.skip(dataType);
        } catch (InvalidBondDataException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }
}
