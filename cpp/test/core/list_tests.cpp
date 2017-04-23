#include "precompiled.h"
#include "serialization_test.h"


template <typename Protocols>
bool Compare(const SimpleListsStruct& left, const SimpleListsStructView& right)
{
    return Compare<Protocols>(left.l_bool, right.l_bool)
        && Compare<Protocols>(left.l_int64, right.l_int64)
        && Compare<Protocols>(left.l_float, right.l_float)
        && Compare<Protocols>(left.l_string, right.l_string)
        && Compare<Protocols>(left.v_int16, right.v_int16)
        && Equal<Protocols>(left.v_double, right.v_double)
        && Compare<Protocols>(left.v_string, right.v_string)
        && Compare<Protocols>(left.s_uint64, right.s_uint64)
        && Compare<Protocols>(left.s_string, right.s_string)
        && Compare<Protocols>(left.m_int8_string, right.m_int8_string)
        && Compare<Protocols>(left.m_string_bool, right.m_string_bool);
}

template <typename Protocols>
bool Compare(const NestedListsStruct& left, const NestedListsStructView& right)
{
    return Equal<Protocols>(left.SLS, right.SLS)
        && Equal<Protocols>(left.vlSLS, right.vlSLS)
        && Equal<Protocols>(left.vvNS, right.vvNS)
        && Compare<Protocols>(left.lsb, right.lsb)
        && Equal<Protocols>(left.vmds, right.vmds);
}


template <uint16_t N, typename Reader, typename Writer>
void SimpleListTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, SimpleListsStruct>(suite, "Simple lists");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, SimpleListsStruct, SimpleListsStructView>(suite, "Simple lists partial view");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, NestedListsStruct>(suite, "Nested lists");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, NestedListsStruct, NestedListsStructView>(suite, "Nested lists partial view");
}

void ListTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        SimpleListTests<
            0x401,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("List deserialization tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        SimpleListTests<
            0x402,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("List deserialization tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        SimpleListTests<
            0x403,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("List deserialization tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        SimpleListTests<
            0x404,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("List deserialization tests for Simple JSON");
    );
}

bool init_unit_test()
{
    ListTestsInit();
    return true;
}
