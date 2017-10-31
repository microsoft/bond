package org.bondlib;

import org.bondlib.Metadata;
import org.bondlib.Modifier;

/**
 * Contains helper methods for working with schema.
 */
public final class SchemaHelper {

    private SchemaHelper() {
    }

    public static byte getDefaultUInt8FieldValue(Metadata fieldMetadata) {
        return (byte) fieldMetadata.default_value.uint_value;
    }

    public static short getDefaultUInt16FieldValue(Metadata fieldMetadata) {
        return (short) fieldMetadata.default_value.uint_value;
    }

    public static int getDefaultUInt32FieldValue(Metadata fieldMetadata) {
        return (int) fieldMetadata.default_value.uint_value;
    }

    public static long getDefaultUInt64FieldValue(Metadata fieldMetadata) {
        return fieldMetadata.default_value.uint_value;
    }

    public static byte getDefaultInt8FieldValue(Metadata fieldMetadata) {
        return (byte) fieldMetadata.default_value.int_value;
    }

    public static short getDefaultInt16FieldValue(Metadata fieldMetadata) {
        return (short) fieldMetadata.default_value.int_value;
    }

    public static int getDefaultInt32FieldValue(Metadata fieldMetadata) {
        return (int) fieldMetadata.default_value.int_value;
    }

    public static long getDefaultInt64FieldValue(Metadata fieldMetadata) {
        return fieldMetadata.default_value.int_value;
    }

    public static boolean getDefaultBoolFieldValue(Metadata fieldMetadata) {
        // false is represented by 0; everything else is considered true
        return fieldMetadata.default_value.int_value != 0;
    }

    public static float getDefaultFloatFieldValue(Metadata fieldMetadata) {
        return (float) fieldMetadata.default_value.double_value;
    }

    public static double getDefaultDoubleFieldValue(Metadata fieldMetadata) {
        return fieldMetadata.default_value.double_value;
    }

    public static String getDefaultStringFieldValue(Metadata fieldMetadata) {
        return fieldMetadata.default_value.string_value;
    }

    public static String getDefaultWStringFieldValue(Metadata fieldMetadata) {
        return fieldMetadata.default_value.wstring_value;
    }
}
