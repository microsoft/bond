#include "custom_protocols.h"
#include "precompiled.h"
#include "serialization_test.h"


template <uint16_t N, typename Reader, typename Writer, typename Protocols>
void CustomProtocolsTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        AllBindingAndMapping1,
        Reader, Writer, TestReaderStruct, Protocols>(suite, "Simple struct");
}


template <uint16_t N, typename Reader, typename Writer, typename Protocols>
void CustomInputBufferTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2,
        Reader, Writer, NestedStruct1, NestedStruct1OptionalBondedView, Protocols>(suite, "Optional bonded field");
}


void CustomProtocolsTestsInit()
{
    TEST_COMPACT_BINARY_PROTOCOL(
        CustomProtocolsTests<
            0x2102,
            unit_test::TestReader<bond::InputBuffer>,
            unit_test::TestWriter<bond::OutputBuffer>,
            bond::BuiltInProtocols::Append<unit_test::TestReader<bond::InputBuffer> > >("Custom protocol TestReader");

        CustomInputBufferTests<
            0x2103,
            unit_test::TestReader<unit_test::CustomInputBuffer>,
            unit_test::TestWriter<bond::OutputBuffer>,
            bond::BuiltInProtocols::Append<unit_test::TestReader<unit_test::CustomInputBuffer> > >("Custom protocol TestReader using CustomInputBuffer");
    );
}

bool init_unit_test()
{
    CustomProtocolsTestsInit();
    return true;
}

