#include "precompiled.h"
#include "skip_tests.h"

template <typename Reader, typename Writer>
struct SkipMismatchedId
{
    template <typename T>
    typename boost::disable_if<std::is_integral<T> >::type
    operator()(const T&)
    {
        // From.field1 and To.field1 have the same type but different ids
        typedef SkipStruct<T>  From;
        typedef SkipStruct2<T> To;

        AllBindingAndMapping<Reader, Writer, From, To>();
    }


    template <typename T>
    typename boost::enable_if<std::is_integral<T> >::type
    operator()(const T&)
    {
        // From.field1 and To.field1 have the same type but different ids
        typedef SkipStruct<T>  From;
        typedef SkipStruct2<T> To;

        // "interesting" integer constants
        const std::vector<T>& constants = IntegerConstants<T>();

        for(size_t i = 0; i < constants.size(); ++i)
        {
            From value;
            value.field1 = constants[i];

            BindingAndMapping<Reader, Writer, From, To>(value);
        }
    }
};


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(SkipMismatchedIdTests)
{
    boost::mpl::for_each<T>(SkipMismatchedId<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void SkipTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        SkipMismatchedIdTests, Reader, Writer, BasicTypes>(suite, "Basic types");

    AddTestCase<TEST_ID(N), 
        SkipMismatchedIdTests, Reader, Writer, SkipTypes<string>::type>(suite, "Complex types");
}


void SkipTest::InitializeMismatchedIdTests()
{
    TEST_SIMPLE_PROTOCOL(
        SkipTests<
            0xa01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Skip mismatched id tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        SkipTests<
            0xa02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Skip mismatched id tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        SkipTests<
            0xa03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Skip mismatched id tests for FastBinary");
    );
}


bool init_unit_test()
{
    SkipTest::InitializeMismatchedIdTests();
    return true;
}

