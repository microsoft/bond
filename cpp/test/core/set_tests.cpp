#include "precompiled.h"
#include "serialization_test.h"

using namespace std;


template <typename Reader, typename Writer>
struct BasicTypesSets
{
    template <typename X>
    void operator()(const X&)
    {
        typedef BondStruct<std::set<X> > T;

        AllBindingAndMapping<Reader, Writer, T>();
    }
};


template <typename T>
typename boost::enable_if<bond::is_basic_type<typename T::value_type>, bool>::type
IsEmpty(const T& x)
{
    return x.empty();
}


template <typename T>
typename boost::enable_if<bond::is_container<typename T::value_type>, bool>::type
IsEmpty(const T& x)
{
    return x.size() == static_cast<size_t>(std::count_if(x.begin(), x.end(), &IsEmpty<typename T::value_type>));
}


// set is incompatible with list or vector
template <typename Reader, typename Writer, typename From, typename To>
void SetAndListFail(const From& from)
{
    BOOST_ASSERT(!from.field.empty());

    // Compile-time schema
    {
        bond::bonded<From> bonded(GetBonded<Reader, Writer, From>(from));

        To to;

        Apply(bond::To<To>(to), bonded);

        UT_AssertIsTrue(IsEmpty(to.field));
    }

    // Runtime schema
    {
        bond::bonded<void> bonded(GetBonded<Reader, Writer, From>(from));

        To to;

        Apply(bond::To<To>(to), bonded);

        UT_AssertIsTrue(IsEmpty(to.field));
    }
}


template <typename Reader, typename Writer>
struct SetAndList
{
    template <typename X>
    void operator()(const X&)
    {
        {
            typedef BondStruct<std::set<X> >     From;
            typedef BondStruct<list<X> >         To;

            SetAndListFail<Reader, Writer, From, To>(InitRandom<From>());
        }

        {
            typedef BondStruct<vector<std::set<X> > > From;
            typedef BondStruct<vector<list<X> > >     To;

            SetAndListFail<Reader, Writer, From, To>(InitRandom<From>());
        }
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(BasicTypesSetTests)
{
    boost::mpl::for_each<SortableTypes>(BasicTypesSets<Reader, Writer>());
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(SetListTests)
{
    boost::mpl::for_each<SortableTypes>(SetAndList<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void SetTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), BasicTypesSetTests, Reader, Writer>(suite, "Basic types sets");

    AddTestCase<TEST_ID(N), SetListTests, Reader, Writer>(suite, "Set-list tests");
}


void SetTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        SetTests<
            0xc01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Set tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        SetTests<
            0xc02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Set tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        SetTests<
            0xc03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Set tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        SetTests<
            0xc04,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Set tests for Simple JSON");
    );
}

bool init_unit_test()
{
    SetTestsInit();
    return true;
}

