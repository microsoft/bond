// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import static org.junit.Assert.*;

public class SomethingTest {

    @Test
    public void testByte() {
        byte value = Byte.MIN_VALUE;
        SomethingByte s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value);
        assertEquals(new Byte(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        byte fallbackValue = Byte.MAX_VALUE;
        assertEquals(value, Something.unwrap(s, fallbackValue));
        assertEquals(fallbackValue, Something.unwrap((SomethingByte) null, fallbackValue));
    }

    @Test
    public void testShort() {
        short value = Short.MIN_VALUE;
        SomethingShort s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value);
        assertEquals(new Short(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        short fallbackValue = Short.MAX_VALUE;
        assertEquals(value, Something.unwrap(s, fallbackValue));
        assertEquals(fallbackValue, Something.unwrap((SomethingShort) null, fallbackValue));
    }

    @Test
    public void testInteger() {
        int value = Integer.MIN_VALUE;
        SomethingInteger s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value);
        assertEquals(new Integer(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        int fallbackValue = Integer.MAX_VALUE;
        assertEquals(value, Something.unwrap(s, fallbackValue));
        assertEquals(fallbackValue, Something.unwrap((SomethingInteger) null, fallbackValue));
    }

    @Test
    public void testLong() {
        long value = Long.MIN_VALUE;
        SomethingLong s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value);
        assertEquals(new Long(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        long fallbackValue = Long.MAX_VALUE;
        assertEquals(value, Something.unwrap(s, fallbackValue));
        assertEquals(fallbackValue, Something.unwrap((SomethingLong) null, fallbackValue));
    }

    @Test
    public void testBoolean() {
        boolean value = true;
        SomethingBoolean s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value);
        assertEquals(new Boolean(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        boolean fallbackValue = false;
        assertEquals(value, Something.unwrap(s, fallbackValue));
        assertEquals(fallbackValue, Something.unwrap((SomethingBoolean) null, fallbackValue));
    }

    @Test
    public void testFloat() {
        float value = Float.MIN_VALUE;
        SomethingFloat s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value, 0);
        assertEquals(new Float(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        float fallbackValue = Float.MAX_VALUE;
        assertEquals(value, Something.unwrap(s, fallbackValue), 0);
        assertEquals(fallbackValue, Something.unwrap((SomethingFloat) null, fallbackValue), 0);
    }

    @Test
    public void testDouble() {
        double value = Double.MIN_VALUE;
        SomethingDouble s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.value, 0);
        assertEquals(new Double(value), s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        double fallbackValue = Double.MAX_VALUE;
        assertEquals(value, Something.unwrap(s, fallbackValue), 0);
        assertEquals(fallbackValue, Something.unwrap((SomethingDouble) null, fallbackValue), 0);
    }

    @Test
    public void testObject() {
        Object value = new Object();
        SomethingObject<Object> s = Something.wrap(value);
        assertNotNull(s);
        assertEquals(value, s.getValue());
        assertEquals(String.valueOf(value), s.toString());
        Object fallbackValue = new String();
        assertEquals(value, Something.unwrap(s, fallbackValue));
        assertEquals(fallbackValue, Something.unwrap((SomethingObject<Object>) null, fallbackValue));
        assertEquals(value, Something.unwrap(s));
        assertNull(Something.unwrap((SomethingObject<Object>) null));
    }
}
