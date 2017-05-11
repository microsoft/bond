
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test

class VarUIntHelperTests {

    // TODO: port to Java and update for stream API

    /*
    val encodeResult = VarUIntHelper.EncodeResult()
    val decodeResult = VarUIntHelper.DecodeResult()

    fun roundTrip16(value: Short) {
        VarUIntHelper.encodeVarInt16(value, encodeResult)
        VarUIntHelper.decodeVarInt16(encodeResult.data, 0, decodeResult)
        Assert.assertEquals("decoded a different value than we encoded",
                value, decodeResult.value.toShort())
    }

    // Test all 2^16 shorts.
    @Test
    fun roundTrips16Exhaustive() {
        for (i in Short.MIN_VALUE..Short.MAX_VALUE) {
            roundTrip16(i.toShort())
        }
    }

    fun roundTrip32(value: Int) {
        VarUIntHelper.encodeVarInt32(value, encodeResult)
        VarUIntHelper.decodeVarInt32(encodeResult.data, 0, decodeResult)
        Assert.assertEquals("decoded a different value than we encoded",
                value, decodeResult.value.toInt())
    }

    // Testing all ints takes ~70 s on a 2015 laptop i5.
    @Ignore
    @Test
    fun roundTrips32Exhaustive() {
        for (i in Int.MIN_VALUE..Int.MAX_VALUE) {
            roundTrip32(i)
        }
    }

    @Test
    fun roundTrips32() {
        // Test ints of all lengths, and their negative selves, alternating
        // adding 1s and 0s.
        roundTrip32(0)
        var x = 1
        for (i in 0 .. 16) {
            roundTrip32(x)
            roundTrip32(-x)
            x *= 2
            roundTrip32(x)
            roundTrip32(-x)
            x = (x + 1) * 2
        }

        // Iterate from -2^31 to 2^31, stepping by 2^24. There will be 2^8
        // values of i, including 0 and Int.MIN_VALUE. This won't cover
        // Int.MAX_VALUE, but read the next comment.
        for (i in Int.MIN_VALUE .. Int.MAX_VALUE step 16777216) {
            // Try 2^8 values centered on i. When i is Int.MIN_VALUE, negative
            // values of j will cover the largest ints, including
            // Int.MAX_VALUE.
            for (j in -127 .. 128) {
                roundTrip32(i + j)
            }
        }
    }

    fun roundTrip64(value: Long) {
        VarUIntHelper.encodeVarInt64(value, encodeResult)
        VarUIntHelper.decodeVarInt64(encodeResult.data, 0, decodeResult)
        Assert.assertEquals("decoded a different value than we encoded",
                value, decodeResult.value)
    }

    // roundTrips64Exhaustive has been omitted for performance reasons.
    // It would take around 10,000 years to complete.

    @Test
    fun roundTrips64() {
        // Test longs of all lengths, and their negative selves, alternating
        // adding 1s and 0s.
        roundTrip64(0)
        var x = 1L
        for (i in 0 .. 32) {
            roundTrip64(x)
            roundTrip64(-x)
            x *= 2
            roundTrip64(x)
            roundTrip64(-x)
            x = (x + 1) * 2
        }

        // Iterate from -2^63 to 2^63, stepping by 2^48. There will be 2^8
        // values of i, including 0 and Long.MIN_VALUE. This won't cover
        // Long.MAX_VALUE, but read the next comment.
        //for (i in Long.MIN_VALUE .. Long.MAX_VALUE step 281474976710656L) {
        for (i in Long.MIN_VALUE .. Long.MAX_VALUE step 281474976710656L) {
            // Try 2^8 values centered on i. When i is Long.MIN_VALUE, negative
            // values of j will cover the largest longs, including
            // Long.MAX_VALUE.
            for (j in -127 .. 128) {
                roundTrip64(i + j)
            }
        }
    }

    */
}