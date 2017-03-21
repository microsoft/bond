package com.microsoft.bond.protocol;

/**
 * Kotlin doesn't have bitwise operators, so we do this in Javaland.
 */
public class VarIntHelper {
    public static final int VarInt16MaxBytes = 3;
    public static final int VarInt32MaxBytes = 5;
    public static final int VarInt64MaxBytes = 10;

    /**
     * Contains up to VarInt64MaxBytes bytes in an array and a count of how many are part of the
     * currently encoded value. All encode/decode varint functions take one of these as an argument
     * to reduce garbage generation.
     */
    public static class Result {
        public final byte[] data = new byte[VarInt64MaxBytes];
        public int length;
    }

    /**
     * @param result output argument - will contain the varint encoding of value
     */
    public static void encodeVarInt16(short value, Result result) {
        result.length = 0;

        // byte 0
        if (value >= 0x80)
        {
            result.data[result.length++] = (byte)(value | 0x80);
            value >>= 7;
            // byte 1
            if (value >= 0x80)
            {
                result.data[result.length++] = (byte)(value | 0x80);
                value >>= 7;
            }
        }
        // byte 2
        result.data[result.length++] = (byte)value;
    }

    /**
     * @param result output argument - will contain the varint encoding of value
     */
    public static void encodeVarInt32(int value, Result result)
    {
        result.length = 0;

        // byte 0
        if (value >= 0x80)
        {
            result.data[result.length++] = (byte)(value | 0x80);
            value >>= 7;
            // byte 1
            if (value >= 0x80)
            {
                result.data[result.length++] = (byte)(value | 0x80);
                value >>= 7;
                // byte 2
                if (value >= 0x80)
                {
                    result.data[result.length++] = (byte)(value | 0x80);
                    value >>= 7;
                    // byte 3
                    if (value >= 0x80)
                    {
                        result.data[result.length++] = (byte)(value | 0x80);
                        value >>= 7;
                    }
                }
            }
        }
        // last byte
        result.data[result.length++] = (byte)value;
    }

    /**
     * @param result output argument - will contain the varint encoding of value
     */
    public static void encodeVarInt64(long value, Result result) {
        result.length = 0;

        // byte 0
        if (value >= 0x80) {
            result.data[result.length++] = (byte) (value | 0x80);
            value >>= 7;
            // byte 1
            if (value >= 0x80) {
                result.data[result.length++] = (byte) (value | 0x80);
                value >>= 7;
                // byte 2
                if (value >= 0x80) {
                    result.data[result.length++] = (byte) (value | 0x80);
                    value >>= 7;
                    // byte 3
                    if (value >= 0x80) {
                        result.data[result.length++] = (byte) (value | 0x80);
                        value >>= 7;
                        // byte 4
                        if (value >= 0x80) {
                            result.data[result.length++] = (byte) (value | 0x80);
                            value >>= 7;
                            // byte 5
                            if (value >= 0x80) {
                                result.data[result.length++] = (byte) (value | 0x80);
                                value >>= 7;
                                // byte 6
                                if (value >= 0x80) {
                                    result.data[result.length++] = (byte) (value | 0x80);
                                    value >>= 7;
                                    // byte 7
                                    if (value >= 0x80) {
                                        result.data[result.length++] = (byte) (value | 0x80);
                                        value >>= 7;
                                        // byte 8
                                        if (value >= 0x80) {
                                            result.data[result.length++] = (byte) (value | 0x80);
                                            value >>= 7;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        // last byte
        result.data[result.length++] = (byte) value;
    }
}
