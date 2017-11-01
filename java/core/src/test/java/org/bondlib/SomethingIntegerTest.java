// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingIntegerTest {

    private static final int[] testValues = new int[]{
            Integer.MIN_VALUE,
            Integer.MIN_VALUE + 1,
            Integer.MIN_VALUE / 2,
            -1,
            0,
            1,
            Integer.MAX_VALUE / 2,
            Integer.MAX_VALUE - 1,
            Integer.MAX_VALUE
    };

    @Test
    public void testConstructorAndMethods() {
        for (int testValue : testValues) {
            SomethingInteger testObject = new SomethingInteger(testValue);
            assertEquals(testValue, testObject.value);
            assertEquals(new Integer(testValue), testObject.getValue());
            int newTestValue = (int) (~testValue);
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value);
            assertEquals(new Integer(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingInteger testObject = new SomethingInteger(0);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (int testValue : testValues) {
            testObjects.add(new SomethingInteger(testValue));
            testObjects.add(new SomethingInteger(testValue));
            testObjects.add(new SomethingInteger(testValue));
            testObjects.add(new SomethingObject<Integer>(testValue));
            testObjects.add(new Integer(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingInteger) {
                    assertFalse(a.equals(null));
                    SomethingInteger av = (SomethingInteger) a;
                    if (b instanceof SomethingInteger) {
                        SomethingInteger bv = (SomethingInteger) b;
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
