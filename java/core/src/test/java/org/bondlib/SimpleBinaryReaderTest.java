// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;

public class SimpleBinaryReaderTest {

    // See SimpleBinaryProtocolTest for more tests (on both reader and writer)

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithZeroProtocolVersion() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        new SimpleBinaryReader(bais, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithInvalidProtocolVersion() {
        ByteArrayInputStream bais = new ByteArrayInputStream(new byte[0]);
        new SimpleBinaryReader(bais, 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorWithNullInputStream() {
        new SimpleBinaryReader(null, 1);
    }
}
