// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class SomethingByteTest {

    private static final byte[] testValues = new byte[]{
            Byte.MIN_VALUE,
            Byte.MIN_VALUE + 1,
            Byte.MIN_VALUE / 2,
            (byte) -1,
            (byte) 0,
            (byte) 1,
            Byte.MAX_VALUE / 2,
            Byte.MAX_VALUE - 1,
            Byte.MAX_VALUE
    };

    @Test
    public void testConstructorAndMethods() {
        for (byte testValue : testValues) {
            SomethingByte testObject = new SomethingByte(testValue);
            assertEquals(testValue, testObject.value);
            assertEquals(new Byte(testValue), testObject.getValue());
            byte newTestValue = (byte) (~testValue);
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value);
            assertEquals(new Byte(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingByte testObject = new SomethingByte((byte) 0);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (byte testValue : testValues) {
            testObjects.add(new SomethingByte(testValue));
            testObjects.add(new SomethingByte(testValue));
            testObjects.add(new SomethingByte(testValue));
            testObjects.add(new SomethingObject<Byte>(testValue));
            testObjects.add(new Byte(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingByte) {
                    assertFalse(a.equals(null));
                    SomethingByte av = (SomethingByte) a;
                    if (b instanceof SomethingByte) {
                        SomethingByte bv = (SomethingByte) b;
                        assertEquals(av.value == bv.value, a.equals(b));
                    } else {
                        assertFalse(a.equals(b));
                    }
                }
            }
        }

        TestHelper.verifyEqualsAndHashCodeConsistency(testObjects);
    }
}
