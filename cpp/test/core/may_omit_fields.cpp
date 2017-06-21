#include "precompiled.h"
#include "serialization_test.h"

// In order to disable omitting optional field only in this unit test, we use
// protocols with a different output buffer types than in all other tests. 
class MyOutputBuffer : public bond::OutputBuffer 
{
public:
    template<typename T>
    void WriteVariableUnsigned(T value)
    {
        bond::OutputBuffer::WriteVariableUnsigned(value);
    }
};


namespace bond
{
    // Override optional field omitting for CompactBinary and FastBinary
    template <> struct
    may_omit_fields<CompactBinaryWriter<MyOutputBuffer> >
        : std::false_type {};


    template <> struct 
    may_omit_fields<FastBinaryWriter<MyOutputBuffer> >
        : std::false_type {};
}


template <typename Reader, typename Writer, typename From, typename To>
TEST_CASE_BEGIN(DontOmit)
{
    From from;

    typename Writer::Buffer output;
    Writer writer(output);
    Serialize(from, writer);

    To to;

    InitRandom(to);

    typename Reader::Buffer input(output.GetBuffer());
    Reader reader(input);
    Deserialize(reader, to);

    UT_Compare(from.field1, to.field1);
    UT_Compare(from.field2, to.field2);
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void MayOmitFieldsTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        DontOmit, Reader, Writer, BondStructOptional<uint64_t>, BondStructRequired<uint64_t> >(suite, "Don't omit optional fields");
};


void MayOmitFieldsTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        MayOmitFieldsTests<
            0x1b01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<MyOutputBuffer> >("may_omit_fields tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        MayOmitFieldsTests<
            0x1b02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<MyOutputBuffer> >("may_omit_fields tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        MayOmitFieldsTests<
            0x1b03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<MyOutputBuffer> >("may_omit_fields tests for FastBinary");
    );
}

bool init_unit_test()
{
    MayOmitFieldsTestsInit();
    return true;
}

