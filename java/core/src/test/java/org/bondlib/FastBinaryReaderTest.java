// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import static org.junit.Assert.fail;

public class FastBinaryReaderTest {

    // See FastBinaryProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        new FastBinaryReader(bais, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        new FastBinaryReader(bais, 2);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullInputStream() {
        ByteArrayInputStream bais = null;
        new FastBinaryReader(bais, 1);
    }

    @Test
    public void testSkipWithInvalidDataType() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        FastBinaryReader reader = new FastBinaryReader(bais, 1);
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
