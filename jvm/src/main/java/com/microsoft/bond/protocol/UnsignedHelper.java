package com.microsoft.bond.protocol;

import java.math.BigInteger;

public class UnsignedHelper {
    private final static BigInteger TWO_TO_64 = BigInteger.valueOf(2L).pow(64);
    private final static BigInteger LONG_MAX_PLUS_ONE = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE);

    public static short asUnsignedShort(byte signed) {
        return (short) (signed & 0xFF);
    }

    public static int asUnsignedInt(short signed) {
        return signed & 0xFFFF;
    }

    public static long asUnsignedLong(int signed) {
        return signed & 0xFFFFFFFFL;
    }

    public static BigInteger asUnsignedBigInt(long signed) {
        if (signed >= 0) {
            return BigInteger.valueOf(signed);
        } else if (signed == Long.MIN_VALUE) {
            return LONG_MAX_PLUS_ONE;
        } else {
            return TWO_TO_64.add(BigInteger.valueOf(signed));
        }
    }
}
