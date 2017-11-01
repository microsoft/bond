// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.Charset;

/**
 * Unit tests for the {@link SimpleJsonReader} class.
 */
public final class SimpleJsonReaderTest {

    private static final Charset UTF8_CHARSET = Charset.forName("UTF-8");

    ///////////////////////////////////////////////////////////////////////////
    // readBool tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadBool() throws IOException {
        parseAndVerifyBool("false", false);
        parseAndVerifyBool("true", true);
    }

    @Test
    public void testReadBool_Error_Integer() throws IOException {
        parseMalformedAndVerifyBool("1");
    }

    @Test
    public void testReadBool_Error_Float() throws IOException {
        parseMalformedAndVerifyBool("1.0");
    }

    @Test
    public void testReadBool_Error_String() throws IOException {
        parseMalformedAndVerifyBool("\"0\"");
    }

    @Test
    public void testReadBool_Error_Literal() throws IOException {
        parseMalformedAndVerifyBool("null");
    }

    @Test
    public void testReadBool_Error_Array() throws IOException {
        parseMalformedAndVerifyBool("[]");
    }

    @Test
    public void testReadBool_Error_Object() throws IOException {
        parseMalformedAndVerifyBool("{}");
    }

    private static void parseAndVerifyBool(String jsonValue, boolean expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        boolean actualScalarField1Value = r.readBool();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        boolean actualScalarField2Value = r.readBool();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        boolean actualArrayField1Element1Value = r.readBool();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        boolean actualArrayField2Element1Value = r.readBool();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        boolean actualArrayField2Element2Value = r.readBool();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyBool(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            boolean value = r.readBool();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readInt8 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadInt8() throws IOException {
        byte[] expectedValues = new byte[]{
                0,
                1,
                2,
                9,
                99,
                Byte.MAX_VALUE - 1,
                Byte.MAX_VALUE,
                -1,
                -2,
                -9,
                -99,
                Byte.MIN_VALUE + 1,
                Byte.MIN_VALUE
        };
        for (byte expectedValue : expectedValues) {
            parseAndVerifyInt8(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadInt8_Error_Overflow() throws IOException {
        parseMalformedAndVerifyInt8(String.valueOf((int) Byte.MAX_VALUE + 1));
    }

    @Test
    public void testReadInt8_Error_Underflow() throws IOException {
        parseMalformedAndVerifyInt8(String.valueOf((int) Byte.MIN_VALUE - 1));
    }

    @Test
    public void testReadInt8_Error_Float() throws IOException {
        parseMalformedAndVerifyInt8("1.0");
    }

    @Test
    public void testReadInt8_Error_String() throws IOException {
        parseMalformedAndVerifyInt8("\"0\"");
    }

    @Test
    public void testReadInt8_Error_Literal() throws IOException {
        parseMalformedAndVerifyInt8("true");
    }

    @Test
    public void testReadInt8_Error_Array() throws IOException {
        parseMalformedAndVerifyInt8("[]");
    }

    @Test
    public void testReadInt8_Error_Object() throws IOException {
        parseMalformedAndVerifyInt8("{}");
    }

    private static void parseAndVerifyInt8(String jsonValue, byte expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        byte actualScalarField1Value = r.readInt8();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        byte actualScalarField2Value = r.readInt8();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        byte actualArrayField1Element1Value = r.readInt8();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        byte actualArrayField2Element1Value = r.readInt8();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        byte actualArrayField2Element2Value = r.readInt8();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyInt8(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            byte value = r.readInt8();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readInt16 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadInt16() throws IOException {
        short[] expectedValues = new short[]{
                0,
                1,
                2,
                9,
                99,
                999,
                9999,
                Short.MAX_VALUE - 1,
                Short.MAX_VALUE,
                -1,
                -2,
                -9,
                -99,
                -999,
                -9999,
                Short.MIN_VALUE + 1,
                Short.MIN_VALUE
        };
        for (short expectedValue : expectedValues) {
            parseAndVerifyInt16(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadInt16_Error_Overflow() throws IOException {
        parseMalformedAndVerifyInt16(String.valueOf((int) Short.MAX_VALUE + 1));
    }

    @Test
    public void testReadInt16_Error_Underflow() throws IOException {
        parseMalformedAndVerifyInt16(String.valueOf((int) Short.MIN_VALUE - 1));
    }

    @Test
    public void testReadInt16_Error_Float() throws IOException {
        parseMalformedAndVerifyInt16("1.0");
    }

    @Test
    public void testReadInt16_Error_String() throws IOException {
        parseMalformedAndVerifyInt16("\"0\"");
    }

    @Test
    public void testReadInt16_Error_Literal() throws IOException {
        parseMalformedAndVerifyInt16("true");
    }

    @Test
    public void testReadInt16_Error_Array() throws IOException {
        parseMalformedAndVerifyInt16("[]");
    }

    @Test
    public void testReadInt16_Error_Object() throws IOException {
        parseMalformedAndVerifyInt16("{}");
    }

    private static void parseAndVerifyInt16(String jsonValue, short expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        short actualScalarField1Value = r.readInt16();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        short actualScalarField2Value = r.readInt16();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        short actualArrayField1Element1Value = r.readInt16();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        short actualArrayField2Element1Value = r.readInt16();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        short actualArrayField2Element2Value = r.readInt16();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyInt16(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            short value = r.readInt16();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readInt32 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadInt32() throws IOException {
        int[] expectedValues = new int[]{
                0,
                1,
                2,
                9,
                99,
                999,
                9999,
                99999,
                999999,
                9999999,
                99999999,
                999999999,
                Integer.MAX_VALUE - 1,
                Integer.MAX_VALUE,
                -1,
                -2,
                -31,
                -99,
                -999,
                -9999,
                -99999,
                -999999,
                -9999999,
                -99999999,
                -999999999,
                Integer.MIN_VALUE + 1,
                Integer.MIN_VALUE
        };
        for (int expectedValue : expectedValues) {
            parseAndVerifyInt32(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadInt32_Error_Overflow() throws IOException {
        parseMalformedAndVerifyInt32(String.valueOf((long) Integer.MAX_VALUE + 1));
    }

    @Test
    public void testReadInt32_Error_Underflow() throws IOException {
        parseMalformedAndVerifyInt32(String.valueOf((long) Integer.MIN_VALUE - 1));
    }

    @Test
    public void testReadInt32_Error_Float() throws IOException {
        parseMalformedAndVerifyInt32("1.0");
    }

    @Test
    public void testReadInt32_Error_String() throws IOException {
        parseMalformedAndVerifyInt32("\"0\"");
    }

    @Test
    public void testReadInt32_Error_Literal() throws IOException {
        parseMalformedAndVerifyInt32("true");
    }

    @Test
    public void testReadInt32_Error_Array() throws IOException {
        parseMalformedAndVerifyInt32("[]");
    }

    @Test
    public void testReadInt32_Error_Object() throws IOException {
        parseMalformedAndVerifyInt32("{}");
    }

    private static void parseAndVerifyInt32(String jsonValue, int expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        int actualScalarField1Value = r.readInt32();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        int actualScalarField2Value = r.readInt32();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        int actualArrayField1Element1Value = r.readInt32();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        int actualArrayField2Element1Value = r.readInt32();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        int actualArrayField2Element2Value = r.readInt32();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyInt32(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            int value = r.readInt32();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    // readInt64 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadInt64() throws IOException {
        long[] expectedValues = new long[]{
                0,
                1,
                2,
                9,
                99,
                999,
                9999,
                99999,
                999999,
                9999999,
                99999999,
                999999999,
                9999999999L,
                99999999999L,
                999999999999L,
                9999999999999L,
                99999999999999L,
                999999999999999L,
                9999999999999999L,
                99999999999999999L,
                999999999999999999L,
                Long.MAX_VALUE - 1,
                Long.MAX_VALUE,
                -1,
                -2,
                -31,
                -99,
                -999,
                -9999,
                -99999,
                -999999,
                -9999999,
                -99999999,
                -999999999,
                -9999999999L,
                -99999999999L,
                -999999999999L,
                -9999999999999L,
                -99999999999999L,
                -999999999999999L,
                -9999999999999999L,
                -99999999999999999L,
                -999999999999999999L,
                Long.MIN_VALUE + 1,
                Long.MIN_VALUE
        };
        for (long expectedValue : expectedValues) {
            parseAndVerifyInt64(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadInt64_Error_Overflow() throws IOException {
        parseMalformedAndVerifyInt64(String.valueOf(new BigInteger(String.valueOf(Long.MAX_VALUE)).add(BigInteger.ONE)));
    }

    @Test
    public void testReadInt64_Error_Underflow() throws IOException {
        parseMalformedAndVerifyInt64(String.valueOf(new BigInteger(String.valueOf(Long.MIN_VALUE)).subtract(BigInteger.ONE)));
    }

    @Test
    public void testReadInt64_Error_Float() throws IOException {
        parseMalformedAndVerifyInt64("1.0");
    }

    @Test
    public void testReadInt64_Error_String() throws IOException {
        parseMalformedAndVerifyInt64("\"0\"");
    }

    @Test
    public void testReadInt64_Error_Literal() throws IOException {
        parseMalformedAndVerifyInt64("true");
    }

    @Test
    public void testReadInt64_Error_Array() throws IOException {
        parseMalformedAndVerifyInt64("[]");
    }

    @Test
    public void testReadInt64_Error_Object() throws IOException {
        parseMalformedAndVerifyInt64("{}");
    }

    private static void parseAndVerifyInt64(String jsonValue, long expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        long actualScalarField1Value = r.readInt64();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        long actualScalarField2Value = r.readInt64();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        long actualArrayField1Element1Value = r.readInt64();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        long actualArrayField2Element1Value = r.readInt64();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        long actualArrayField2Element2Value = r.readInt64();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyInt64(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            long value = r.readInt64();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readUInt8 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadUInt8() throws IOException {
        int[] expectedValues = new int[]{
                0,
                1,
                2,
                9,
                99,
                Byte.MAX_VALUE - 1,
                Byte.MAX_VALUE,
                Byte.MAX_VALUE + 1,
                UnsignedHelper.MAX_UINT8_VALUE - 1,
                UnsignedHelper.MAX_UINT8_VALUE,

        };
        for (int expectedValue : expectedValues) {
            parseAndVerifyUInt8(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadUInt8_Error_Overflow() throws IOException {
        parseMalformedAndVerifyUInt8(String.valueOf(UnsignedHelper.MAX_UINT8_VALUE + 1));
    }

    @Test
    public void testReadUInt8_Error_Negative() throws IOException {
        parseMalformedAndVerifyUInt8("-1");
    }

    @Test
    public void testReadUInt8_Error_NegativeOverflow() throws IOException {
        parseMalformedAndVerifyUInt8("-" + String.valueOf(UnsignedHelper.MAX_UINT8_VALUE + 1));
    }

    @Test
    public void testReadUInt8_Error_Float() throws IOException {
        parseMalformedAndVerifyUInt8("1.0");
    }

    @Test
    public void testReadUInt8_Error_String() throws IOException {
        parseMalformedAndVerifyUInt8("\"0\"");
    }

    @Test
    public void testReadUInt8_Error_Literal() throws IOException {
        parseMalformedAndVerifyUInt8("true");
    }

    @Test
    public void testReadUInt8_Error_Array() throws IOException {
        parseMalformedAndVerifyUInt8("[]");
    }

    @Test
    public void testReadUInt8_Error_Object() throws IOException {
        parseMalformedAndVerifyUInt8("{}");
    }


    private static void parseAndVerifyUInt8(String jsonValue, int expectedValueUnsigned) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        byte actualScalarField1Value = r.readUInt8();
        int actualScalarField1ValueUnsigned = UnsignedHelper.asUnsignedInt(actualScalarField1Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField1ValueUnsigned);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        byte actualScalarField2Value = r.readUInt8();
        int actualScalarField2ValueUnsigned = UnsignedHelper.asUnsignedInt(actualScalarField2Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField2ValueUnsigned);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        byte actualArrayField1Element1Value = r.readUInt8();
        int actualArrayField1Element1ValueUnsigned = UnsignedHelper.asUnsignedInt(actualArrayField1Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField1Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        byte actualArrayField2Element1Value = r.readUInt8();
        int actualArrayField2Element1ValueUnsigned = UnsignedHelper.asUnsignedInt(actualArrayField2Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        byte actualArrayField2Element2Value = r.readUInt8();
        int actualArrayField2Element2ValueUnsigned = UnsignedHelper.asUnsignedInt(actualArrayField2Element2Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element2ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyUInt8(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            byte value = r.readUInt8();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }
    ///////////////////////////////////////////////////////////////////////////
    // readUInt16 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadUInt16() throws IOException {
        int[] expectedValues = new int[]{
                0,
                1,
                2,
                9,
                99,
                999,
                9999,
                Short.MAX_VALUE - 1,
                Short.MAX_VALUE,
                Short.MAX_VALUE + 1,
                UnsignedHelper.MAX_UINT16_VALUE - 1,
                UnsignedHelper.MAX_UINT16_VALUE

        };
        for (int expectedValue : expectedValues) {
            parseAndVerifyUInt16(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadUInt16_Error_Overflow() throws IOException {
        parseMalformedAndVerifyUInt16(String.valueOf(UnsignedHelper.MAX_UINT16_VALUE + 1));
    }

    @Test
    public void testReadUInt16_Error_Negative() throws IOException {
        parseMalformedAndVerifyUInt16("-1");
    }

    @Test
    public void testReadUInt16_Error_NegativeOverflow() throws IOException {
        parseMalformedAndVerifyUInt16("-" + String.valueOf(UnsignedHelper.MAX_UINT16_VALUE + 1));
    }

    @Test
    public void testReadUInt16_Error_Float() throws IOException {
        parseMalformedAndVerifyUInt16("1.0");
    }

    @Test
    public void testReadUInt16_Error_String() throws IOException {
        parseMalformedAndVerifyUInt16("\"0\"");
    }

    @Test
    public void testReadUInt16_Error_Literal() throws IOException {
        parseMalformedAndVerifyUInt16("true");
    }

    @Test
    public void testReadUInt16_Error_Array() throws IOException {
        parseMalformedAndVerifyUInt16("[]");
    }

    @Test
    public void testReadUInt16_Error_Object() throws IOException {
        parseMalformedAndVerifyUInt16("{}");
    }

    private static void parseAndVerifyUInt16(String jsonValue, int expectedValueUnsigned) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        short actualScalarField1Value = r.readUInt16();
        int actualScalarField1ValueUnsigned = UnsignedHelper.asUnsignedInt(actualScalarField1Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField1ValueUnsigned);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        short actualScalarField2Value = r.readUInt16();
        int actualScalarField2ValueUnsigned = UnsignedHelper.asUnsignedInt(actualScalarField2Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField2ValueUnsigned);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        short actualArrayField1Element1Value = r.readUInt16();
        int actualArrayField1Element1ValueUnsigned = UnsignedHelper.asUnsignedInt(actualArrayField1Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField1Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        short actualArrayField2Element1Value = r.readUInt16();
        int actualArrayField2Element1ValueUnsigned = UnsignedHelper.asUnsignedInt(actualArrayField2Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        short actualArrayField2Element2Value = r.readUInt16();
        int actualArrayField2Element2ValueUnsigned = UnsignedHelper.asUnsignedInt(actualArrayField2Element2Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element2ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyUInt16(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            short value = r.readUInt16();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readUInt32 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadUInt32() throws IOException {
        long[] expectedValues = new long[]{
                0,
                1,
                2,
                9,
                99,
                999,
                9999,
                99999,
                999999,
                9999999,
                99999999,
                999999999,
                Integer.MAX_VALUE - 1,
                Integer.MAX_VALUE,
                UnsignedHelper.MAX_UINT32_VALUE - 1L,
                UnsignedHelper.MAX_UINT32_VALUE

        };
        for (long expectedValue : expectedValues) {
            parseAndVerifyUInt32(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadUInt32_Error_Overflow() throws IOException {
        parseMalformedAndVerifyUInt32(String.valueOf(UnsignedHelper.MAX_UINT32_VALUE + 1));
    }

    @Test
    public void testReadUInt32_Error_Negative() throws IOException {
        parseMalformedAndVerifyUInt32("-1");
    }

    @Test
    public void testReadUInt32_Error_NegativeOverflow() throws IOException {
        parseMalformedAndVerifyUInt32("-" + String.valueOf(UnsignedHelper.MAX_UINT32_VALUE + 1));
    }

    @Test
    public void testReadUInt32_Error_Float() throws IOException {
        parseMalformedAndVerifyUInt32("1.0");
    }

    @Test
    public void testReadUInt32_Error_String() throws IOException {
        parseMalformedAndVerifyUInt32("\"0\"");
    }

    @Test
    public void testReadUInt32_Error_Literal() throws IOException {
        parseMalformedAndVerifyUInt32("true");
    }

    @Test
    public void testReadUInt32_Error_Array() throws IOException {
        parseMalformedAndVerifyUInt32("[]");
    }

    @Test
    public void testReadUInt32_Error_Object() throws IOException {
        parseMalformedAndVerifyUInt32("{}");
    }

    private static void parseAndVerifyUInt32(String jsonValue, long expectedValueUnsigned) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        int actualScalarField1Value = r.readUInt32();
        long actualScalarField1ValueUnsigned = UnsignedHelper.asUnsignedLong(actualScalarField1Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField1ValueUnsigned);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        int actualScalarField2Value = r.readUInt32();
        long actualScalarField2ValueUnsigned = UnsignedHelper.asUnsignedLong(actualScalarField2Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField2ValueUnsigned);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        int actualArrayField1Element1Value = r.readUInt32();
        long actualArrayField1Element1ValueUnsigned = UnsignedHelper.asUnsignedLong(actualArrayField1Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField1Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        int actualArrayField2Element1Value = r.readUInt32();
        long actualArrayField2Element1ValueUnsigned = UnsignedHelper.asUnsignedLong(actualArrayField2Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        int actualArrayField2Element2Value = r.readUInt32();
        long actualArrayField2Element2ValueUnsigned = UnsignedHelper.asUnsignedLong(actualArrayField2Element2Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element2ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyUInt32(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            int value = r.readUInt32();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readUInt64 tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadUInt64() throws IOException {
        BigInteger[] expectedValues = new BigInteger[]{
                BigInteger.ZERO,
                BigInteger.ONE,
                new BigInteger("2"),
                new BigInteger("9"),
                new BigInteger("99"),
                new BigInteger("999"),
                new BigInteger("9999"),
                new BigInteger("99999"),
                new BigInteger("999999"),
                new BigInteger("9999999"),
                new BigInteger("99999999"),
                new BigInteger("999999999"),
                new BigInteger("9999999999"),
                new BigInteger("99999999999"),
                new BigInteger("999999999999"),
                new BigInteger("9999999999999"),
                new BigInteger("99999999999999"),
                new BigInteger("999999999999999"),
                new BigInteger("9999999999999999"),
                new BigInteger("99999999999999999"),
                new BigInteger("999999999999999999"),
                new BigInteger(String.valueOf(Long.MAX_VALUE)).subtract(BigInteger.ONE),
                new BigInteger(String.valueOf(Long.MAX_VALUE)),
                UnsignedHelper.MAX_UINT64_VALUE.subtract(BigInteger.ONE),
                UnsignedHelper.MAX_UINT64_VALUE

        };
        for (BigInteger expectedValue : expectedValues) {
            parseAndVerifyUInt64(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadUInt64_Error_Overflow() throws IOException {
        parseMalformedAndVerifyUInt64(String.valueOf(UnsignedHelper.MAX_UINT64_VALUE.add(BigInteger.ONE)));
    }

    @Test
    public void testReadUInt64_Error_Negative() throws IOException {
        parseMalformedAndVerifyUInt64("-1");
    }

    @Test
    public void testReadUInt64_Error_NegativeOverflow() throws IOException {
        parseMalformedAndVerifyUInt64("-" + String.valueOf(UnsignedHelper.MAX_UINT64_VALUE.add(BigInteger.ONE)));
    }

    @Test
    public void testReadUInt64_Error_Float() throws IOException {
        parseMalformedAndVerifyUInt64("1.0");
    }

    @Test
    public void testReadUInt64_Error_String() throws IOException {
        parseMalformedAndVerifyUInt64("\"0\"");
    }

    @Test
    public void testReadUInt64_Error_Literal() throws IOException {
        parseMalformedAndVerifyUInt64("true");
    }

    @Test
    public void testReadUInt64_Error_Array() throws IOException {
        parseMalformedAndVerifyUInt64("[]");
    }

    @Test
    public void testReadUInt64_Error_Object() throws IOException {
        parseMalformedAndVerifyUInt64("{}");
    }

    private static void parseAndVerifyUInt64(String jsonValue, BigInteger expectedValueUnsigned) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        long actualScalarField1Value = r.readUInt64();
        BigInteger actualScalarField1ValueUnsigned = UnsignedHelper.asUnsignedBigInt(actualScalarField1Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField1ValueUnsigned);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        long actualScalarField2Value = r.readUInt64();
        BigInteger actualScalarField2ValueUnsigned = UnsignedHelper.asUnsignedBigInt(actualScalarField2Value);
        Assert.assertEquals(expectedValueUnsigned, actualScalarField2ValueUnsigned);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        long actualArrayField1Element1Value = r.readUInt64();
        BigInteger actualArrayField1Element1ValueUnsigned = UnsignedHelper.asUnsignedBigInt(actualArrayField1Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField1Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        long actualArrayField2Element1Value = r.readUInt64();
        BigInteger actualArrayField2Element1ValueUnsigned = UnsignedHelper.asUnsignedBigInt(actualArrayField2Element1Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element1ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        long actualArrayField2Element2Value = r.readUInt64();
        BigInteger actualArrayField2Element2ValueUnsigned = UnsignedHelper.asUnsignedBigInt(actualArrayField2Element2Value);
        Assert.assertEquals(expectedValueUnsigned, actualArrayField2Element2ValueUnsigned);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyUInt64(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            long value = r.readUInt64();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readFloat tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadFloat() throws IOException {
        float[] expectedValues = new float[]{
                0f,
                -0f,
                1f,
                -1f,
                1.0f,
                -1.0f,
                1.0000000000000000000001f,
                -1.0000000000000000000001f,
                222333444555f,
                -222333444555f,
                1.1e10f,
                -1.1e10f,
                1.1e-10f,
                -1.1e-10f,
                0.1e30f,
                -0.1e30f,
                0.1e-30f,
                -0.1e-30f,
                Float.MIN_VALUE,
                Float.MAX_VALUE,
                Float.NEGATIVE_INFINITY,
                Float.POSITIVE_INFINITY,
                Float.NaN
        };
        for (float expectedValue : expectedValues) {
            parseAndVerifyFloat(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadFloat_Error_String() throws IOException {
        parseMalformedAndVerifyFloat("\"0\"");
    }

    @Test
    public void testReadFloat_Error_Literal() throws IOException {
        parseMalformedAndVerifyFloat("true");
    }

    @Test
    public void testReadFloat_Error_Array() throws IOException {
        parseMalformedAndVerifyFloat("[]");
    }

    @Test
    public void testReadFloat_Error_Object() throws IOException {
        parseMalformedAndVerifyFloat("{}");
    }

    private static void parseAndVerifyFloat(String jsonValue, float expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;
        float tolerance = 0;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        float actualScalarField1Value = r.readFloat();
        Assert.assertEquals(expectedValue, actualScalarField1Value, tolerance);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        float actualScalarField2Value = r.readFloat();
        Assert.assertEquals(expectedValue, actualScalarField2Value, tolerance);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        float actualArrayField1Element1Value = r.readFloat();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value, tolerance);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        float actualArrayField2Element1Value = r.readFloat();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value, tolerance);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        float actualArrayField2Element2Value = r.readFloat();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value, tolerance);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyFloat(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            float value = r.readFloat();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readDouble tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadDouble() throws IOException {
        double[] expectedValues = new double[]{
                0d,
                -0d,
                1d,
                -1d,
                1.0d,
                -1.0d,
                1.0000000000000000000001d,
                -1.0000000000000000000001d,
                222333444555d,
                -222333444555d,
                1.1e10d,
                -1.1e10d,
                1.1e-10d,
                -1.1e-10d,
                0.1e300d,
                -0.1e300d,
                0.1e-300d,
                -0.1e-300d,
                Double.MIN_VALUE,
                Double.MAX_VALUE,
                Double.NEGATIVE_INFINITY,
                Double.POSITIVE_INFINITY,
                Double.NaN
        };
        for (double expectedValue : expectedValues) {
            parseAndVerifyDouble(String.valueOf(expectedValue), expectedValue);
        }
    }

    @Test
    public void testReadDouble_Error_String() throws IOException {
        parseMalformedAndVerifyDouble("\"0\"");
    }

    @Test
    public void testReadDouble_Error_Literal() throws IOException {
        parseMalformedAndVerifyDouble("true");
    }

    @Test
    public void testReadDouble_Error_Array() throws IOException {
        parseMalformedAndVerifyDouble("[]");
    }

    @Test
    public void testReadDouble_Error_Object() throws IOException {
        parseMalformedAndVerifyDouble("{}");
    }

    private static void parseAndVerifyDouble(String jsonValue, double expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;
        double tolerance = 0;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        double actualScalarField1Value = r.readDouble();
        Assert.assertEquals(expectedValue, actualScalarField1Value, tolerance);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        double actualScalarField2Value = r.readDouble();
        Assert.assertEquals(expectedValue, actualScalarField2Value, tolerance);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        double actualArrayField1Element1Value = r.readDouble();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value, tolerance);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        double actualArrayField2Element1Value = r.readDouble();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value, tolerance);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        double actualArrayField2Element2Value = r.readDouble();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value, tolerance);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyDouble(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            double value = r.readDouble();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readString tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadString() throws IOException {
        String[] expectedValues = new String[]{
                "",
                "unit test",
                "\u55ae\u5143\u6e2c\u8a66",
                "\u30e6\u30cb\u30c3\u30c8\u30c6\u30b9\u30c8",
                "\u0a2f\u0a42\u0a28\u0a3f\u0a1f \u0a1f\u0a48\u0a38\u0a1f",
                "\u043c\u043e\u0434\u0443\u043b\u044c\u043d\u044b\u0439 \u0442\u0435\u0441\u0442",
                "Ki\u1ec3m tra \u0111\u01a1n v\u1ecb",
                "\u00fcnite testi",
                "\u10d4\u10e0\u10d7\u10d4\u10e3\u10da\u10d8 \u10d2\u10d0\u10db\u10dd\u10ea\u10d3\u10d0",
                "\u05de\u05d1\u05d7\u05df \u05d9\u05d7\u05d9\u05d3\u05d4",
                "\u0627\u062e\u062a\u0628\u0627\u0631 \u0627\u0644\u0648\u062d\u062f\u0629",
                "\uD852\uDF62",
        };
        for (String expectedValue : expectedValues) {
            parseAndVerifyString('\"' + expectedValue + '\"', expectedValue);
        }
    }

    @Test
    public void testReadString_Error_Integer() throws IOException {
        parseMalformedAndVerifyString("1");
    }

    @Test
    public void testReadString_Error_Float() throws IOException {
        parseMalformedAndVerifyString("1.0");
    }

    @Test
    public void testReadString_Error_Literal() throws IOException {
        parseMalformedAndVerifyString("null");
    }

    @Test
    public void testReadString_Error_Array() throws IOException {
        parseMalformedAndVerifyString("[]");
    }

    @Test
    public void testReadString_Error_Object() throws IOException {
        parseMalformedAndVerifyString("{}");
    }

    private static void parseAndVerifyString(String jsonValue, String expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        String actualScalarField1Value = r.readString();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        String actualScalarField2Value = r.readString();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        String actualArrayField1Element1Value = r.readString();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        String actualArrayField2Element1Value = r.readString();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        String actualArrayField2Element2Value = r.readString();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyString(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            String value = r.readString();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // readWString tests
    ///////////////////////////////////////////////////////////////////////////

    @Test
    public void testReadWString() throws IOException {
        String[] expectedValues = new String[]{
                "",
                "unit test",
                "\u55ae\u5143\u6e2c\u8a66",
                "\u30e6\u30cb\u30c3\u30c8\u30c6\u30b9\u30c8",
                "\u0a2f\u0a42\u0a28\u0a3f\u0a1f \u0a1f\u0a48\u0a38\u0a1f",
                "\u043c\u043e\u0434\u0443\u043b\u044c\u043d\u044b\u0439 \u0442\u0435\u0441\u0442",
                "Ki\u1ec3m tra \u0111\u01a1n v\u1ecb",
                "\u00fcnite testi",
                "\u10d4\u10e0\u10d7\u10d4\u10e3\u10da\u10d8 \u10d2\u10d0\u10db\u10dd\u10ea\u10d3\u10d0",
                "\u05de\u05d1\u05d7\u05df \u05d9\u05d7\u05d9\u05d3\u05d4",
                "\u0627\u062e\u062a\u0628\u0627\u0631 \u0627\u0644\u0648\u062d\u062f\u0629",
                "\uD852\uDF62",
        };
        for (String expectedValue : expectedValues) {
            parseAndVerifyWString('\"' + expectedValue + '\"', expectedValue);
        }
    }

    @Test
    public void testReadWString_Error_Integer() throws IOException {
        parseMalformedAndVerifyWString("1");
    }

    @Test
    public void testReadWString_Error_Float() throws IOException {
        parseMalformedAndVerifyWString("1.0");
    }

    @Test
    public void testReadWString_Error_Literal() throws IOException {
        parseMalformedAndVerifyWString("null");
    }

    @Test
    public void testReadWString_Error_Array() throws IOException {
        parseMalformedAndVerifyWString("[]");
    }

    @Test
    public void testReadWString_Error_Object() throws IOException {
        parseMalformedAndVerifyWString("{}");
    }

    private static void parseAndVerifyWString(String jsonValue, String expectedValue) throws IOException {
        // build JSON object with 4 fields: two scalars and two arrays (with 1 element and 2 elements)
        String scalarField1Name = "scalarField1";
        String scalarField2Name = "scalarField2";
        String arrayField1Name = "arrayField1";
        String arrayField2Name = "arrayField2";
        String json = "{" +
                "\"" + scalarField1Name + "\":" + jsonValue + "," +
                "\"" + scalarField2Name + "\":" + jsonValue + "," +
                "\"" + arrayField1Name + "\":[" + jsonValue + "]," +
                "\"" + arrayField2Name + "\":[" + jsonValue + "," + jsonValue + "]" + "}";

        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();

        String currentFieldName;
        boolean currentContainerHasItems;

        // scalar field 1
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField1Name, currentFieldName);
        String actualScalarField1Value = r.readWString();
        Assert.assertEquals(expectedValue, actualScalarField1Value);
        r.readFieldEnd();

        // scalar field 2
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(scalarField2Name, currentFieldName);
        String actualScalarField2Value = r.readWString();
        Assert.assertEquals(expectedValue, actualScalarField2Value);
        r.readFieldEnd();

        // array field 1 (1 element)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField1Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        String actualArrayField1Element1Value = r.readWString();
        Assert.assertEquals(expectedValue, actualArrayField1Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        // array field 2 (2 elements)
        currentFieldName = r.readFieldBegin();
        Assert.assertEquals(arrayField2Name, currentFieldName);
        r.readContainerBegin();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        String actualArrayField2Element1Value = r.readWString();
        Assert.assertEquals(expectedValue, actualArrayField2Element1Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertTrue(currentContainerHasItems);
        String actualArrayField2Element2Value = r.readWString();
        Assert.assertEquals(expectedValue, actualArrayField2Element2Value);
        r.readContainerItemEnd();
        currentContainerHasItems = r.readContainerItemBegin();
        Assert.assertFalse(currentContainerHasItems);
        r.readContainerEnd();
        r.readFieldEnd();

        currentFieldName = r.readFieldBegin();
        Assert.assertNull(currentFieldName);
        r.readStructEnd();
    }

    private static void parseMalformedAndVerifyWString(String jsonValue) throws IOException {
        String fieldName = "field";
        String json = "{\"" + fieldName + "\":" + jsonValue + "}";
        ByteArrayInputStream bais = new ByteArrayInputStream(json.getBytes(UTF8_CHARSET));
        SimpleJsonReader r = new SimpleJsonReader(bais);
        r.readStructBegin();
        String currentFieldName = r.readFieldBegin();
        Assert.assertEquals(fieldName, currentFieldName);
        try {
            String value = r.readWString();
            Assert.fail("Parsing should fail: " + value);
        } catch (InvalidBondDataException e) {
            // success
        }
    }
}
