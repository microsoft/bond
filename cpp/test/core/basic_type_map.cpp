#include "precompiled.h"
#include "serialization_test.h"

using namespace std;


template <typename Reader, typename Writer>
struct MapKeyTests
{
    template <typename X>
    void operator()(const X&)
    {
        typedef BondStruct<map<X, string> > T;

        AllBindingAndMapping<Reader, Writer, T>();
    }
};


template <typename Reader, typename Writer>
struct MapValueTests
{
    template <typename X>
    void operator()(const X&)
    {
        typedef BondStruct<map<string, X> > T;

        AllBindingAndMapping<Reader, Writer, T>();
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(BasicTypesMaps)
{
    boost::mpl::for_each<SortableTypes>(MapKeyTests<Reader, Writer>());
    boost::mpl::for_each<BasicTypes>(MapValueTests<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void BasicTypesMapTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), BasicTypesMaps, Reader, Writer>(suite, "Maps with basic type key/value");
}


void BasicTypesMapTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        BasicTypesMapTests<
            0xf01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Basic types maps for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        BasicTypesMapTests<
            0xf02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Basic types maps for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        BasicTypesMapTests<
            0xf03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Basic types maps for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        BasicTypesMapTests<
            0xf04,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Basic types maps for Simple JSON");
    );
}

bool init_unit_test()
{
    BasicTypesMapTestsInit();
    return true;
}

