// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.protocol.*;
import org.junit.Assert;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.*;
import java.nio.ByteBuffer;
import java.util.*;

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

    /**
     * Given a collection of objects, verifies consistencies between methods equals and hashCode.
     *
     * @param testObjects objects to test
     */
    public static void verifyEqualsAndHashCodeConsistency(Collection<?> testObjects) {
        for (Object a : testObjects) {
            // single object tests
            assertTrue("Object equality must be reflexive", a.equals(a));

            for (Object b : testObjects) {
                // two object tests
                assertEquals("Object equality must be symmetric", a.equals(b), b.equals(a));
                if (a.equals(b)) {
                    assertEquals("For equal objects the hash codes must be equal", a.hashCode(), b.hashCode());
                }
                if (a.hashCode() != b.hashCode()) {
                    assertFalse("Objects with different hash codes must not be equal", a.equals(b));
                }

                for (Object c : testObjects) {
                    // three object tests
                    if (a.equals(b) && b.equals(c)) {
                        assertTrue("Object equality must be transitive", a.equals(c));
                    }
                }
            }
        }
    }

    /**
     * Converts raw int bits to float.
     *
     * @param bits raw bits
     * @return reinterpreted float
     */
    public static float rawIntBitsToFloat(int bits) {
        return ByteBuffer.allocate(4).putInt(bits).getFloat(0);
    }

    /**
     * Converts raw long bits to double.
     *
     * @param bits raw bits
     * @return reinterpreted double
     */
    public static double rawLongBitsToDouble(long bits) {
        return ByteBuffer.allocate(8).putLong(bits).getDouble(0);
    }

    /**
     * Marshals a Bond struct instance using given protocol and version, then unmarshals it
     * and tests whether the original instance is equal to the unmarshaled one.
     * @param obj struct instance
     * @param protocolType protocol type
     * @param protocolVersion protocol version
     * @throws IOException if error occurred
     */
    public static void marshalUnmarshalAndCompare(
            BondSerializable obj,
            ProtocolType protocolType,
            int protocolVersion) throws IOException {
        // create protocol writer
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ProtocolWriter protocolWriter;
        switch (protocolType.value) {
            case ProtocolType.Values.FAST_PROTOCOL:
                protocolWriter = new FastBinaryWriter(baos, protocolVersion);
                break;
            case ProtocolType.Values.COMPACT_PROTOCOL:
                protocolWriter = new CompactBinaryWriter(baos, protocolVersion);
                break;
            case ProtocolType.Values.SIMPLE_PROTOCOL:
                protocolWriter = new SimpleBinaryWriter(baos, protocolVersion);
                break;
            default:
                throw new IllegalArgumentException("Unsupported protocol type: " + protocolType);
        }
        // marshall/unmarshal/compare
        Marshal.marshal(obj, protocolWriter);
        byte[] payloadBytes = baos.toByteArray();
        ByteArrayInputStream bais = new ByteArrayInputStream(payloadBytes);
        Bonded<? extends BondSerializable> bondedCloneObj = Unmarshal.unmarshal(bais, obj.getBondType());
        BondSerializable cloneObj = bondedCloneObj.deserialize();
        boolean areEqual = obj.equals(cloneObj);
        Assert.assertTrue(areEqual);
    }

    /**
     * Performs memberwise comparison of two Bond struct objects of the same Bond struct type
     * (and same Java class). Raises assertion error with details if the comparison fails.
     *
     * @param a         struct object
     * @param b         struct object
     * @param <TStruct> struct type
     * @throws IllegalAccessException if there was a reflection error
     */
    public static <TStruct extends BondSerializable> void assertStructMemberwiseEquals(
            TStruct a, TStruct b) throws IllegalAccessException {
        assertStructMemberwiseEqualsHelper(a, b, "");
    }

    private static void assertStructMemberwiseEqualsHelper(
            Object a, Object b, String path) throws IllegalAccessException {
        if (a != null && b == null) {
            fail("If first object is non-null then the second object must not be null: " + path);
        } else if (a == null && b != null) {
            fail("If second object is non-null then the first object must not be null: " + path);
        } else if (a == null && b == null) {
            // nothing more to compare
            return;
        }

        assertSame("Object class must be the same: " + path, a.getClass(), b.getClass());

        Class<?> clazz = a.getClass();
        if (BondSerializable.class.isAssignableFrom(clazz)) {
            // struct type
            Field[] fields = clazz.getFields();
            for (Field field : fields) {
                if (!java.lang.reflect.Modifier.isStatic(field.getModifiers())) {
                    // compare field values based onthe field's declared type
                    Class<?> fieldClass = field.getType();
                    String fieldPath = path + "." + field.getName();
                    if (fieldClass == Byte.TYPE) {
                        // primitive type
                        assertEquals("Fields of type byte must match: " + fieldPath,
                                field.getByte(a), field.getByte(b));
                    } else if (fieldClass == Short.TYPE) {
                        // primitive type
                        assertEquals("Fields of type short must match: " + fieldPath,
                                field.getShort(a), field.getShort(b));
                    } else if (fieldClass == Integer.TYPE) {
                        // primitive type
                        assertEquals("Fields of type int must match: " + fieldPath,
                                field.getInt(a), field.getInt(b));
                    } else if (fieldClass == Long.TYPE) {
                        // primitive type
                        assertEquals("Fields of type long must match: " + fieldPath,
                                field.getLong(a), field.getLong(b));
                    } else if (fieldClass == Boolean.TYPE) {
                        // primitive type
                        assertEquals("Fields of type boolean must match: " + fieldPath,
                                field.getBoolean(a), field.getBoolean(b));
                    } else if (fieldClass == Float.TYPE) {
                        // primitive type
                        assertEquals("Fields of type float must match: " + fieldPath,
                                field.getFloat(a), field.getFloat(b), 0F);
                    } else if (fieldClass == Long.TYPE) {
                        // primitive type
                        assertEquals("Fields of type double must match: " + fieldPath,
                                field.getDouble(a), field.getDouble(b), 0D);
                    } else {
                        // object
                        assertStructMemberwiseEqualsHelper(field.get(a), field.get(b), fieldPath);
                    }
                }
            }
        } else if (List.class.isAssignableFrom(clazz)) {
            // list type
            List aList = (List)a;
            List bList = (List)b;
            assertEquals("List sizes muct mathch: " + path, aList.size(), bList.size());
            for (int i = 0; i < aList.size(); ++i) {
                String elementPath = path + "[" + i + "]";
                assertStructMemberwiseEqualsHelper(aList.get(i), bList.get(i), elementPath);
            }
        } else if (Set.class.isAssignableFrom(clazz)) {
            // set
            // TODO: support set comparison
            throw new UnsupportedOperationException();
        } else if (Map.class.isAssignableFrom(clazz)) {
            // map
            // TODO: support map comparison
            throw new UnsupportedOperationException();
        } else if (Bonded.class.isAssignableFrom(clazz)) {
            // bonded
            // TODO: support bonded comparison
            throw new UnsupportedOperationException();
        } else {
            // general object
            assertEquals("Values of type " + clazz.getName() + " must match: " + path, a, b);
        }
    }
}