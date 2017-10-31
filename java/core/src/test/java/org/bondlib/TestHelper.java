// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
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
     *
     * @param obj             struct instance
     * @param protocolType    protocol type
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
        try {
            assertStructMemberwiseEquals(obj, cloneObj);
        } catch (IllegalAccessException ex) {
            // shouldn't happen
            fail("Unexpected exception raised when testing marshal/unmarshal roundtrip: " + ex);
        }
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
            TStruct a, TStruct b) throws IllegalAccessException, IOException {
        ArgumentHelper.ensureNotNull(a, "a");
        ArgumentHelper.ensureNotNull(b, "b");
        assertStructMemberwiseEqualsHelper(a, b, "");
    }

    // helper for assertStructMemberwiseEquals, which compares two field values
    private static void assertStructMemberwiseEqualsHelper(
            Object a, Object b, String path) throws IllegalAccessException, IOException {
        // deal with nulls first
        assertEquals("Ether both values are null or none: " + path, a == null, b == null);
        if (a != null && b != null) {
            Class aValueClass = a.getClass();
            Class bValueClass = b.getClass();
            // The following Bond types require special equality test, all others use the "equals" method:
            // * structs - needs to report which fields differ and recurse
            // * collections (list/vector, set, map) - needs to report which elements differ and recurse
            // * blob - needs to report which elements differ
            // * bonded - need to implement custom equality comparison
            if (BondSerializable.class.isAssignableFrom(aValueClass)) {
                // struct type
                assertSame("Both objects must be the same struct: " + path, aValueClass, bValueClass);
                Field[] fields = aValueClass.getFields();
                for (Field field : fields) {
                    if (!java.lang.reflect.Modifier.isStatic(field.getModifiers())) {
                        String fieldPath = path + "." + field.getName();
                        Object aFieldValue = field.get(a);
                        Object bFieldValue = field.get(b);
                        assertStructMemberwiseEqualsHelper(aFieldValue, bFieldValue, fieldPath);
                    }
                }
            } else if (List.class.isAssignableFrom(aValueClass)) {
                // list or vector type
                assertTrue("Both objects must be lists: " + path, List.class.isAssignableFrom(bValueClass));
                List<?> aList = (List) a;
                List<?> bList = (List) b;
                assertEquals("List sizes must match: " + path, aList.size(), bList.size());
                for (int i = 0; i < aList.size(); ++i) {
                    String elementPath = path + "[" + i + "]";
                    assertStructMemberwiseEqualsHelper(aList.get(i), bList.get(i), elementPath);
                }
            } else if (Set.class.isAssignableFrom(aValueClass)) {
                // set type (elements are restricted to primitive Bond types so no need to recurse)
                assertTrue("Both objects must be sets: " + path, Set.class.isAssignableFrom(bValueClass));
                Set<?> aSet = (Set) a;
                Set<?> bSet = (Set) b;
                assertEquals("Set sizes must match: " + path, aSet.size(), bSet.size());
                for (Object aSetElement : aSet) {
                    assertTrue(
                            "Element " + aSetElement + " of the first set must in the second set: " + path,
                            bSet.contains(aSetElement));
                }
            } else if (Map.class.isAssignableFrom(aValueClass)) {
                // map type (elements are restricted to primitive Bond types so no need to recurse for keys)
                assertTrue("Both objects must be maps: " + path, Map.class.isAssignableFrom(bValueClass));
                Map<?, ?> aMap = (Map) a;
                Map<?, ?> bMap = (Map) b;
                assertEquals("Map sizes must match: " + path, aMap.size(), bMap.size());
                for (Map.Entry aMapEntry : aMap.entrySet()) {
                    Object aMapKey = aMapEntry.getKey();
                    Object aMapValue = aMapEntry.getValue();
                    assertTrue(
                            "Key " + aMapKey + " of the first map must be in the second map: " + path,
                            bMap.containsKey(aMapKey));
                    Object bMapValue = bMap.get(aMapKey);
                    String elementPath = path + "[" + aMapKey + "]";
                    assertStructMemberwiseEqualsHelper(aMapValue, bMapValue, elementPath);
                }
            } else if (Bonded.class.isAssignableFrom(aValueClass)) {
                // bonded type, instances of which may have different struct types but the same data, which
                // happens when deserializing bonded<Base> that was originally serialized as bonded<Derived>
                assertTrue("Both objects must be bondeds: " + path, Bonded.class.isAssignableFrom(bValueClass));
                Bonded<?> aBonded = (Bonded) a;
                Bonded<?> bBonded = (Bonded) b;
                StructBondType<?> aBondedType = aBonded.getBondType();
                StructBondType<?> bBondedType = bBonded.getBondType();
                assertTrue(
                        "One type must be a subtype of another: " + path,
                        aBondedType.isSubtypeOf(bBondedType) || bBondedType.isSubtypeOf(aBondedType));
                // compare Bonded data by comparing deserialized as base and separately as derived structs
                Object aBondedStructAsA = aBonded.deserialize(aBondedType);
                Object bBondedStructAsA = bBonded.deserialize(aBondedType);
                Object aBondedStructAsB = aBonded.deserialize(bBondedType);
                Object bBondedStructAsB = bBonded.deserialize(bBondedType);
                assertStructMemberwiseEqualsHelper(aBondedStructAsA, bBondedStructAsA, path);
                assertStructMemberwiseEqualsHelper(aBondedStructAsB, bBondedStructAsB, path);
            } else {
                // everything else, for which the equals method is sufficient:
                // (float/double values are boxed and the equals implementation of the box works as expected)
                assertSame("Both objects must be of the same class: " + path, aValueClass, bValueClass);
                assertEquals("Values must match: " + path, a, b);
            }
        }
    }
}