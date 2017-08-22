// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingLongTest {

    private static final long[] testValues = new long[]{
            Long.MIN_VALUE,
            Long.MIN_VALUE + 1,
            Long.MIN_VALUE / 2,
            -1L,
            0L,
            1L,
            Long.MAX_VALUE / 2,
            Long.MAX_VALUE - 1,
            Long.MAX_VALUE
    };

    @Test
    public void testConstructorAndMethods() {
        for (long testValue : testValues) {
            SomethingLong testObject = new SomethingLong(testValue);
            assertEquals(testValue, testObject.value);
            assertEquals(new Long(testValue), testObject.getValue());
            long newTestValue = (long) (~testValue);
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value);
            assertEquals(new Long(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingLong testObject = new SomethingLong(0L);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (long testValue : testValues) {
            testObjects.add(new SomethingLong(testValue));
            testObjects.add(new SomethingLong(testValue));
            testObjects.add(new SomethingLong(testValue));
            testObjects.add(new SomethingObject<Long>(testValue));
            testObjects.add(new Long(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingLong) {
                    assertFalse(a.equals(null));
                    SomethingLong av = (SomethingLong) a;
                    if (b instanceof SomethingLong) {
                        SomethingLong bv = (SomethingLong) b;
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
