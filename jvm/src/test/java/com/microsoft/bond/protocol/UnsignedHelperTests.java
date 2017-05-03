package com.microsoft.bond.protocol;

import org.junit.Assert;
import org.junit.Test;

import java.math.BigInteger;

import static com.microsoft.bond.protocol.UnsignedHelper.*;

public class UnsignedHelperTests {
    @Test
    public void unsignedByte() {
        Assert.assertEquals((short) 0, asUnsignedShort((byte) 0));
        Assert.assertEquals((short) 1, asUnsignedShort((byte) 1));
        // signed byte max
        Assert.assertEquals((short) 127, asUnsignedShort((byte) 127));
        Assert.assertEquals((short) 128, asUnsignedShort((byte) -128));
        // unsigned byte max
        Assert.assertEquals((short) 255, asUnsignedShort((byte) -1));
    }

    @Test
    public void unsignedShort() {
        Assert.assertEquals(0, asUnsignedInt((short) 0));
        Assert.assertEquals(1, asUnsignedInt((short) 1));
        // signed short max
        Assert.assertEquals(32767, asUnsignedInt((short) 32767));
        Assert.assertEquals(32768, asUnsignedInt((short) -32768));
        // unsigned short max
        Assert.assertEquals(65535, asUnsignedInt((short) -1));
    }

    @Test
    public void unsignedInt() {
        Assert.assertEquals(0, asUnsignedLong(0));
        Assert.assertEquals(1, asUnsignedLong(1));
        // signed int max
        Assert.assertEquals(2147483647, asUnsignedLong(2147483647));
        Assert.assertEquals(2147483648L, asUnsignedLong(-2147483648));
        // unsigned int max
        Assert.assertEquals(4294967295L, asUnsignedLong(-1));
    }

    @Test
    public void unsignedLong() {
        Assert.assertEquals(BigInteger.ZERO, asUnsignedBigInt(0));
        Assert.assertEquals(BigInteger.ONE, asUnsignedBigInt(1));
        // signed long max
        Assert.assertEquals(new BigInteger("9223372036854775807"), asUnsignedBigInt(Long.MAX_VALUE));
        Assert.assertEquals(new BigInteger("9223372036854775808"), asUnsignedBigInt(Long.MIN_VALUE));
        // unsigned long max
        Assert.assertEquals(new BigInteger("18446744073709551615"), asUnsignedBigInt(-1));
    }
}