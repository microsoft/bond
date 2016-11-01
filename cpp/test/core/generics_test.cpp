#include "precompiled.h"
#include "serialization_test.h"


template <uint16_t N, typename Reader, typename Writer>
void GenericStructTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        AllBindingAndMapping1, Reader, Writer, Generic<SimpleBase, uint16_t, string, Generic<NestedStruct, EnumType1, bool, float> > >(suite, "Generic struct");

    AddTestCase<TEST_ID(N), 
        AllBindingAndMapping1, Reader, Writer, Generic2<float> >(suite, "Generic2 struct");
}


void GenericsTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        GenericStructTests<
            0x1701,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Generics tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        GenericStructTests<
            0x1702,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Generics tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        GenericStructTests<
            0x1703,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Generics tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        GenericStructTests<
            0x1704,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Generics tests for Simple JSON");
    );
}

bool init_unit_test()
{
    GenericsTestsInit();
    return true;
}

