#include "precompiled.h"
#include "serialization_test.h"
#include "numeric_conversions.h"

template <typename Reader, typename Writer>
struct EnumConverter
{
    template <typename Num1>
    void operator()(const Num1&)
    {
        // if int32_t is incompatible with Num1, then enum is incompatible with Num1 as well
        // otherwise enum is compatible with Num1 
        typename boost::mpl::if_<boost::mpl::contains<typename incompatible_types<Num1>::type, int32_t>,
            NumericConverterFail<Reader, Writer, Num1>,
            NumericConverter2<Reader, Writer, Num1>
        >::type()(EnumValue3);
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(EnumConversions)
{
    // types compatible with int32 are compatible with enums
    boost::mpl::for_each<compatible_types<int32_t>::type>(NumericConverter2<Reader, Writer, EnumType1>());
    // types incompatible with int32 are incompatible with enums
    boost::mpl::for_each<incompatible_types<int32_t>::type>(NumericConverterFail<Reader, Writer, EnumType1>());
    
    // tests converting enum to numeric types
    boost::mpl::for_each<NumericTypes>(EnumConverter<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void EnumConversionsTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), EnumConversions, Reader, Writer>(suite, "Enum conversions");
}


void EnumConversionTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        EnumConversionsTests<
            0xe01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Enum conversion tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        EnumConversionsTests<
            0xe02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Enum conversion tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        EnumConversionsTests<
            0xe03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Enum conversion tests for FastBinary");
    );
}

bool init_unit_test()
{
    EnumConversionTestsInit();
    return true;
}

