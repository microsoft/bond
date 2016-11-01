#include "precompiled.h"
#include "serialization_test.h"

using namespace std;


template <typename Reader, typename Writer>
struct ListTests
{
    template <typename X>
    void operator()(const X&)
    {
        typedef BondStruct<X> T;

        AllBindingAndMapping<Reader, Writer, T>();
    }
};


template <typename Reader, typename Writer>
struct ListGenerator
{
    template <typename T>
    void operator()(const T&)
    {
        boost::mpl::for_each<typename ListTypes<T>::type>(ListTests<Reader, Writer>());
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(BasicTypesLists)
{
    // Construct type list containing all basic types as well as list containers of string, int32 and float.
    // The unit test will test list containers of every type from that type list.
    typedef boost::mpl::copy<BasicTypes, boost::mpl::front_inserter<ListTypes<string>::type> >::type  Types1;

    boost::mpl::for_each<Types1>(ListGenerator<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void BasicTypesListTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), BasicTypesLists, Reader, Writer>(suite, "Basic types lists");
}


void BasicTypesListTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        BasicTypesListTests<
            0x101,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Basic types lists for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        BasicTypesListTests<
            0x102,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Basic types lists for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        BasicTypesListTests<
            0x103,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Basic types lists for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        BasicTypesListTests<
            0x104,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Basic types lists for Simple JSON");
    );
}

bool init_unit_test()
{
    BasicTypesListTestsInit();
    return true;
}

