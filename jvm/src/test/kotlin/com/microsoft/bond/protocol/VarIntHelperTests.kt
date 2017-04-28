
import com.microsoft.bond.protocol.VarIntHelper
import org.junit.Assert
import org.junit.Test

typealias VarIntDecoder = (ByteArray, Int, VarIntHelper.DecodeResult) -> Unit

class VarIntHelperTests {
    val encodeResult = VarIntHelper.EncodeResult()
    val decodeResult = VarIntHelper.DecodeResult()

    fun roundTrip16(value: Short) {
        VarIntHelper.encodeVarInt16(value, encodeResult)
        VarIntHelper.decodeVarInt16(encodeResult.data, 0, decodeResult)
        Assert.assertEquals("decoded a different value than we encoded",
                value, decodeResult.value.toShort())
    }

    @Test
    fun roundTrips16() {
        // one byte
        roundTrip16(0)
        roundTrip16(1)
        roundTrip16(127)
        // two bytes
        roundTrip16(128)
        roundTrip16(16383)
        // three bytes
        roundTrip16(32767)
        roundTrip16(-1)
        roundTrip16(-128)
        roundTrip16(-32768)
    }
}