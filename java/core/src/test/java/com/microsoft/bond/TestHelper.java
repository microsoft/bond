// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Contains helper methods for implementing unit tests.
 */
public final class TestHelper {

    // prevent instantiation
    private TestHelper() {
    }

    /**
     * Verifies that the argument class meets a set of informal conditions for a "static class", which are:
     * <ul>
     * <li>
     * The class is a concrete class (not an abstract class or an interface), and
     * </li>
     * <li>
     * The class declares a parameterless constructor with private visibility, and
     * </li>
     * <li>
     * The class doesn't declare any instance members other than the single constructor above, and
     * </li>
     * <li>
     * The class is declared as final.
     * </li>
     * </ul>
     * The method raises an {@link AssertionError} if any of the conditions above are not met. It also invokes
     * the private parameterless constructor to make sure it's covered by unit tests.
     *
     * @param clazz the class to verify
     */
    public static void verifyStaticHelperClass(Class<?> clazz) {

        // The class is a concrete class (not an abstract class or an interface)
        assertFalse(
                "Static class must not be an abstract class",
                java.lang.reflect.Modifier.isAbstract(clazz.getModifiers()));
        assertFalse(
                "Static class must not be an interface",
                java.lang.reflect.Modifier.isInterface(clazz.getModifiers()));

        // The class declares a parameterless constructor with private visibility
        Constructor<?>[] ctors = clazz.getDeclaredConstructors();
        assertEquals("Static class must have only a single constructor", 1, ctors.length);
        Constructor<?> ctor = ctors[0];
        assertEquals("Static class's single constructor must be parameterless", 0, ctor.getParameterTypes().length);
        assertTrue(
                "Static class's single constructor must be private",
                java.lang.reflect.Modifier.isPrivate(ctor.getModifiers()));

        // The class doesn't declare any instance members other than the single constructor above
        ArrayList<Member> fieldsAndMethods = new ArrayList<Member>();
        fieldsAndMethods.addAll(Arrays.asList(clazz.getDeclaredFields()));
        fieldsAndMethods.addAll(Arrays.asList(clazz.getDeclaredMethods()));
        for (Member m : fieldsAndMethods) {
            assertTrue(
                    "Static class must not have non-static fields and methods",
                    java.lang.reflect.Modifier.isStatic(m.getModifiers()));
        }

        // The class is declared as final
        assertTrue(
                "Static class must be declared final",
                java.lang.reflect.Modifier.isFinal(clazz.getModifiers()));

        // Invoke the private constructor to make sure it's included in code coverage
        ctor.setAccessible(true);
        try {
            ctor.newInstance();
        } catch (InvocationTargetException ex) {
            // ok, the private constructor is not expected to be called and can throw anything it wants
            // (the preferred convention however is that the constructor is a no-op)
        } catch (IllegalAccessException ex) {
            // shouldn't happen
            fail("Unexpected exception raised when testing static class: " + ex);
        } catch (InstantiationException ex) {
            // shouldn't happen
            fail("Unexpected exception raised when testing static class: " + ex);
        }
    }
}
