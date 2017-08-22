// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingObjectTest {

    private static final Object[] testValues = new Object[]{
            new Byte((byte)0),
            new Short((short)0),
            new Integer(0),
            new Long(0L),
            new Boolean(false),
            new Float(0F),
            new Double(0D),
            null
    };

    @Test
    public void testConstructorAndMethods() {
        for (Object testValue : testValues) {
            SomethingObject<Object> testObject = new SomethingObject<Object>(testValue);
            assertEquals(testValue, testObject.getValue());
            Object newTestValue = String.valueOf(testValue) + "/";
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.getValue());
        }
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (Object testValue : testValues) {
            testObjects.add(new SomethingObject<Object>(testValue));
            testObjects.add(new SomethingObject<Object>(testValue));
            testObjects.add(new SomethingObject<Object>(testValue));
            testObjects.add(new SomethingObject<Object>(new SomethingObject<Object>(testValue)));
            testObjects.add(new SomethingObject<Object>(null));
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
