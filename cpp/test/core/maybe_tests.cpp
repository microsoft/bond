#include "precompiled.h"
#include "maybe_tests.h"


void ConstInterface(const bond::maybe<int8_t>& x)
{
    bond::maybe<int8_t> z;
    int8_t y;

    UT_AssertIsFalse(x.is_nothing());
    UT_AssertIsFalse(z == x);

    y = x;
        
    UT_AssertIsTrue(y == x);

    y = x.value();
        
    UT_AssertIsTrue(x == y);
    UT_AssertIsFalse(z == x);
    UT_AssertIsTrue(z != x);

    z = x;

    UT_AssertIsTrue(z == x);
    UT_AssertIsFalse(z != x);
}


TEST_CASE_BEGIN(MaybeInterface)
{
    bond::maybe<int8_t> x;
    int8_t y;

    UT_AssertIsTrue(x.is_nothing());
    UT_AssertThrows(y = x, bond::CoreException);
    UT_AssertThrows(x.value(), bond::CoreException);
   
    x.set_value() = 10;

    ConstInterface(x);

    UT_AssertIsFalse(x.is_nothing());

    y = x;

    UT_AssertIsTrue(y == x);

    y = x.value();

    UT_AssertIsTrue(x == y);

    x.set_nothing();

    UT_AssertIsTrue(x.is_nothing());

    x = y;

    UT_AssertIsTrue(x == y);

    bond::maybe<int8_t> z;

    std::swap(x, z);

    UT_AssertIsFalse(z == x);
    UT_AssertIsTrue(z == y);
    UT_AssertIsTrue(x.is_nothing());

    x = z;

    UT_AssertIsTrue(z == x);

    bond::maybe<std::map<double, std::string> > map;

    UT_AssertIsTrue(map.is_nothing());

    map.set_value()[3.14] = "test";

    UT_AssertIsFalse(map.is_nothing());
    UT_AssertIsFalse(map.value().empty());

    std::map<double, std::string> map2;

    std::swap(map2, map.value());
    std::swap(map.value(), map2);

    MaybeStruct<double> m1;
    MaybeStruct<double> m2(m1);

    UT_AssertIsTrue(m1 == m2);

    m1.field = 3.14;

    UT_AssertIsFalse(m1 == m2);

    std::swap(m2, m1);

    UT_AssertIsFalse(m1 == m2);
    UT_AssertIsTrue(m1.field.is_nothing());

    m1 = m2;

    UT_AssertIsTrue(m1 == m2);

    MaybeStruct<double> m3(m1);

    UT_AssertIsTrue(m3 == m2);
}
TEST_CASE_END


template <typename Reader, typename Writer, typename Enable = void>
struct MaybeBindingAndMapping;


template <typename Reader, typename Writer>
struct MaybeBindingAndMapping<Reader, Writer, typename boost::enable_if<bond::may_omit_fields<Writer> >::type>
{
    template <typename X>
    void operator()(const X&)
    {
        {
            typedef MaybeStructOptional<X> T;

            AllBindingAndMapping<Reader, Writer, T>();
        }

        {
            typedef MaybeStruct<X> T;

            UT_AssertThrows((Binding<Reader, Writer, T, T, T>(T())), bond::CoreException);
        }
    }
};


template <typename Reader, typename Writer>
struct MaybeBindingAndMapping<Reader, Writer, typename boost::disable_if<bond::may_omit_fields<Writer> >::type>
{
    template <typename X>
    void operator()(const X&)
    {
        {
            typedef MaybeStruct<X> T;

            Binding<Reader, Writer, T, T, T>(InitRandom<T>());
            Mapping<Reader, Writer, T, T, T>(InitRandom<T>());

            UT_AssertThrows((Binding<Reader, Writer, T, T, T>(T())), bond::CoreException);
        }

        {
            typedef MaybeStructOptional<X> T;

            UT_AssertThrows((Binding<Reader, Writer, T, T, T>(T())), bond::CoreException);
        }
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(MaybeDeserialization)
{
    typedef boost::mpl::copy<BasicTypes, boost::mpl::front_inserter<ListTypes<string>::type> >::type  Types;

    boost::mpl::for_each<Types>(MaybeBindingAndMapping<Reader, Writer>());

    Binding<Reader, Writer, Nothing, Nothing, Nothing>(InitRandom<Nothing>());
    Mapping<Reader, Writer, Nothing, Nothing, Nothing>(InitRandom<Nothing>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void MaybeTests(const char* name)
{
    UnitTestSuite suite(name);
    
    AddTestCase<TEST_ID(N), MaybeDeserialization, Reader, Writer>(suite, "maybe de/serialization");
}


void MaybeTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        MaybeTests<
            0x1a01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Maybe tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        MaybeTests<
            0x1a02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Maybe tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        MaybeTests<
            0x1a03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Maybe tests for FastBinary");
    );

    UnitTestSuite suite("Protocol independent maybe tests");

    AddTestCase<TEST_ID(0x1a05), MaybeInterface>(suite, "APIs");
}


bool init_unit_test()
{
    MaybeTest::Initialize();
    return true;
}

