// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingShortTest {

    private static final short[] testValues = new short[]{
            Short.MIN_VALUE,
            Short.MIN_VALUE + 1,
            Short.MIN_VALUE / 2,
            (short) -1,
            (short) 0,
            (short) 1,
            Short.MAX_VALUE / 2,
            Short.MAX_VALUE - 1,
            Short.MAX_VALUE
    };

    @Test
    public void testConstructorAndMethods() {
        for (short testValue : testValues) {
            SomethingShort testObject = new SomethingShort(testValue);
            assertEquals(testValue, testObject.value);
            assertEquals(new Short(testValue), testObject.getValue());
            short newTestValue = (short) (~testValue);
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value);
            assertEquals(new Short(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingShort testObject = new SomethingShort((short) 0);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (short testValue : testValues) {
            testObjects.add(new SomethingShort(testValue));
            testObjects.add(new SomethingShort(testValue));
            testObjects.add(new SomethingShort(testValue));
            testObjects.add(new SomethingObject<Short>(testValue));
            testObjects.add(new Short(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingShort) {
                    assertFalse(a.equals(null));
                    SomethingShort av = (SomethingShort) a;
                    if (b instanceof SomethingShort) {
                        SomethingShort bv = (SomethingShort) b;
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
