#include "custom_protocols.h"

namespace bond
{
    // Enable TestReader in this file
    template <typename Buffer> struct 
    is_protocol_enabled<unit_test::TestReader<Buffer> >
        : std::true_type {};
}


#include "precompiled.h"
#include "serialization_test.h"


template <uint16_t N, typename Reader, typename Writer>
void CustomProtocolsTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        AllBindingAndMapping1, Reader, Writer, TestReaderStruct>(suite, "Simple struct");
}


void CustomProtocolsTestsInit()
{
    TEST_COMPACT_BINARY_PROTOCOL(
        CustomProtocolsTests<
            0x2102,
            unit_test::TestReader<bond::InputBuffer>,
            unit_test::TestWriter<bond::OutputBuffer> >("Custom protocol TestReader");
    );
}

bool init_unit_test()
{
    CustomProtocolsTestsInit();
    return true;
}

