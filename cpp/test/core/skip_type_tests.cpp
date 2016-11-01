#include "precompiled.h"
#include "skip_tests.h"
#include "non_matching_types.h"

template <typename Reader, typename Writer, typename T1>
struct SkipMismatchedTypes
{
    template <typename T2>
    void operator()(const T2&)
    {
        BOOST_STATIC_ASSERT((!bond::is_matching<T1, T2>::value));
        
        // From.field1 and To.field1 have the same id but non-matching types
        typedef SkipStruct<T1> From;
        typedef SkipStruct<T2> To;

        AllBindingAndMapping<Reader, Writer, From, To>();
    }
};


template <typename Reader, typename Writer>
struct SkipMismatchedType
{
    template <typename T>
    void operator()(const T&)
    {
        boost::mpl::for_each<typename non_matching_types<T>::type>(SkipMismatchedTypes<Reader, Writer, T>());
    }
};


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(SkipMismatchedTypeTests)
{
    boost::mpl::for_each<T>(SkipMismatchedType<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void SkipTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        SkipMismatchedTypeTests, Reader, Writer, BasicTypes>(suite, "Basic types");

    AddTestCase<TEST_ID(N), 
        SkipMismatchedTypeTests, Reader, Writer, SkipTypes<double>::type>(suite, "Complex types");
}


void SkipTest::InitializeMismatchedTypeTests()
{
    TEST_SIMPLE_PROTOCOL(
        SkipTests<
            0x1101,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Skip mismatched type tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        SkipTests<
            0x1102,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Skip mismatched type tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        SkipTests<
            0x1103,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Skip mismatched type tests for FastBinary");
    );
}


bool init_unit_test()
{
    SkipTest::InitializeMismatchedTypeTests();
    return true;
}

