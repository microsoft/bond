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
    public static class EncodeResult {
        public final byte[] data = new byte[VarInt64MaxBytes];
        public int length;
    }

    /**
     * Contains an integer and a count of how many bytes it used when varint-encoded. All
     * encode/decode varint functions take one of these as an argument to reduce garbage
     * generation.
     */
    public static class DecodeResult {
        public long value;
        public int length;
    }

    /**
     * @param result output argument - will contain the varint encoding of value
     */
    public static void encodeVarInt16(short value, EncodeResult result) {
        int unsignedValue = value & 0xFFFF;
        result.length = 0;

        // byte 0
        if (unsignedValue >= 0x80)
        {
            result.data[result.length++] = (byte)(unsignedValue | 0x80);
            unsignedValue >>>= 7;
            // byte 1
            if (unsignedValue >= 0x80)
            {
                result.data[result.length++] = (byte)(unsignedValue | 0x80);
                unsignedValue >>>= 7;
            }
        }
        // byte 2
        result.data[result.length++] = (byte)unsignedValue;
    }

    /**
     * @param result output argument - will contain the varint encoding of value
     */
    public static void encodeVarInt32(int value, EncodeResult result)
    {
        long unsignedValue = value & 0xFFFFFF;
        result.length = 0;

        // byte 0
        if (unsignedValue >= 0x80)
        {
            result.data[result.length++] = (byte)(unsignedValue | 0x80);
            unsignedValue >>>= 7;
            // byte 1
            if (unsignedValue >= 0x80)
            {
                result.data[result.length++] = (byte)(unsignedValue | 0x80);
                unsignedValue >>>= 7;
                // byte 2
                if (unsignedValue >= 0x80)
                {
                    result.data[result.length++] = (byte)(unsignedValue | 0x80);
                    unsignedValue >>>= 7;
                    // byte 3
                    if (unsignedValue >= 0x80)
                    {
                        result.data[result.length++] = (byte)(unsignedValue | 0x80);
                        unsignedValue >>>= 7;
                    }
                }
            }
        }
        // last byte
        result.data[result.length++] = (byte)unsignedValue;
    }

    /**
     * @param result output argument - will contain the varint encoding of value
     */
    public static void encodeVarInt64(long value, EncodeResult result) {
        // FIXME: Need special handling for longs < 0 (i.e., ulongs > LONG_MAX).
        result.length = 0;

        // byte 0
        if (value >= 0x80) {
            result.data[result.length++] = (byte) (value | 0x80);
            value >>>= 7;
            // byte 1
            if (value >= 0x80) {
                result.data[result.length++] = (byte) (value | 0x80);
                value >>>= 7;
                // byte 2
                if (value >= 0x80) {
                    result.data[result.length++] = (byte) (value | 0x80);
                    value >>>= 7;
                    // byte 3
                    if (value >= 0x80) {
                        result.data[result.length++] = (byte) (value | 0x80);
                        value >>>= 7;
                        // byte 4
                        if (value >= 0x80) {
                            result.data[result.length++] = (byte) (value | 0x80);
                            value >>>= 7;
                            // byte 5
                            if (value >= 0x80) {
                                result.data[result.length++] = (byte) (value | 0x80);
                                value >>>= 7;
                                // byte 6
                                if (value >= 0x80) {
                                    result.data[result.length++] = (byte) (value | 0x80);
                                    value >>>= 7;
                                    // byte 7
                                    if (value >= 0x80) {
                                        result.data[result.length++] = (byte) (value | 0x80);
                                        value >>>= 7;
                                        // byte 8
                                        if (value >= 0x80) {
                                            result.data[result.length++] = (byte) (value | 0x80);
                                            value >>>= 7;
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

    /**
     * @param data the byte array containing the varint
     * @param index the index at which the varint begins
     * @param decodeResult output argument - will contain the value and the
     *                     number of bytes it occupied when it was encoded
     */
    public static void decodeVarInt16(byte[] data, int index, DecodeResult decodeResult)
    {
        int i = index;
        // byte 0
        int result = data[i++] & 0xFF;
        if (0x80 <= result)
        {
            // byte 1
            int raw = data[i++] & 0xFF;
            result = (result & 0x7F) | ((raw & 0x7F) << 7);
            if (0x80 <= raw)
            {
                // byte 2
                raw = data[i++] & 0xFF;
                result |= raw << 14;
            }
        }
        decodeResult.length = i - index;
        decodeResult.value = (short) result;
    }

    /**
     * @param data the byte array containing the varint
     * @param index the index at which the varint begins
     * @param decodeResult output argument - will contain the value and the
     *                     number of bytes it occupied when it was encoded
     */
    public static void decodeVarInt32(byte[] data, int index, DecodeResult decodeResult)
    {
        int i = index;
        // byte 0
        int result = data[i++] & 0xFF;
        if (0x80 <= result)
        {
            // byte 1
            int raw = data[i++] & 0xFF;
            result = (result & 0x7F) | ((raw & 0x7F) << 7);
            if (0x80 <= raw)
            {
                // byte 2
                raw = data[i++] & 0xFF;
                result |= (raw & 0x7F) << 14;
                if (0x80 <= raw)
                {
                    // byte 3
                    raw = data[i++] & 0xFF;
                    result |= (raw & 0x7F) << 21;
                    if (0x80 <= raw)
                    {
                        // byte 4
                        raw = data[i++] & 0xFF;
                        result |= raw << 28;
                    }
                }
            }
        }
        decodeResult.length = i - index;
        decodeResult.value = (int) result;
    }

    /**
     * @param data the byte array containing the varint
     * @param index the index at which the varint begins
     * @param decodeResult output argument - will contain the value and the
     *                     number of bytes it occupied when it was encoded
     */
    public static void decodeVarInt64(byte[] data, int index, DecodeResult decodeResult)
    {
        int i = index;
        // byte 0
        long result = data[i++] & 0xFF;
        if (0x80 <= result)
        {
            // byte 1
            long raw = data[i++] & 0xFF;
            result = (result & 0x7F) | ((raw & 0x7F) << 7);
            if (0x80 <= raw)
            {
                // byte 2
                raw = data[i++] & 0xFF;
                result |= (raw & 0x7F) << 14;
                if (0x80 <= raw)
                {
                    // byte 3
                    raw = data[i++] & 0xFF;
                    result |= (raw & 0x7F) << 21;
                    if (0x80 <= raw)
                    {
                        // byte 4
                        raw = data[i++] & 0xFF;
                        result |= (raw & 0x7F) << 28;
                        if (0x80 <= raw)
                        {
                            // byte 5
                            raw = data[i++] & 0xFF;
                            result |= (raw & 0x7F) << 35;
                            if (0x80 <= raw)
                            {
                                // byte 6
                                raw = data[i++] & 0xFF;
                                result |= (raw & 0x7F) << 42;
                                if (0x80 <= raw)
                                {
                                    // byte 7
                                    raw = data[i++] & 0xFF;
                                    result |= (raw & 0x7F) << 49;
                                    if (0x80 <= raw)
                                    {
                                        // byte 8
                                        raw = data[i++] & 0xFF;
                                        result |= raw << 56;
                                        if (0x80 <= raw)
                                        {
                                            // byte 9
                                            i++;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        decodeResult.length = i - index;
        decodeResult.value = result;
    }
}
