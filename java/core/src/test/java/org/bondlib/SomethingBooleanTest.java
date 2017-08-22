// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingBooleanTest {

    private static final boolean[] testValues = new boolean[]{
            false,
            true
    };

    @Test
    public void testConstructorAndMethods() {
        for (boolean testValue : testValues) {
            SomethingBoolean testObject = new SomethingBoolean(testValue);
            assertEquals(testValue, testObject.value);
            assertEquals(new Boolean(testValue), testObject.getValue());
            boolean newTestValue = !testValue;
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value);
            assertEquals(new Boolean(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingBoolean testObject = new SomethingBoolean(false);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (boolean testValue : testValues) {
            testObjects.add(new SomethingBoolean(testValue));
            testObjects.add(new SomethingBoolean(testValue));
            testObjects.add(new SomethingBoolean(testValue));
            testObjects.add(new SomethingObject<Boolean>(testValue));
            testObjects.add(new Boolean(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingBoolean) {
                    assertFalse(a.equals(null));
                    SomethingBoolean av = (SomethingBoolean) a;
                    if (b instanceof SomethingBoolean) {
                        SomethingBoolean bv = (SomethingBoolean) b;
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
