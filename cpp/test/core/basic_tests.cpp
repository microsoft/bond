#include <bond/core/bond_version.h>

namespace bond
{
    // Use Simple Protocol v2 by default
    template <typename Buffer> struct
    default_version<SimpleBinaryReader<Buffer> >
    {
        static const uint16_t value = 2;
    };
};


#include "precompiled.h"
#include "basic_tests.h"


// Checks meta info is correct for a struct derived from BaseWithMeta
template <typename T>
std::string QualifiedName(const T&)
{
    return T::Schema::metadata.qualified_name;
}


template <typename T>
std::string Name(const T&)
{
    return T::Schema::metadata.name;
}

#define CheckMetaInfoMatches(item)  \
    UT_AssertAreEqual(QualifiedName(item), item._base_meta_full_name_10); \
    UT_AssertAreEqual(Name(item), item._base_meta_name_20); \


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(MetaTests)
{
    {
        BaseWithMeta base;
        DeriverFromBaseWithMeta derived;

        CheckMetaInfoMatches(base);
        CheckMetaInfoMatches(derived);
    }

    {
        BaseWithMeta base;
        DeriverFromBaseWithMeta derived;

        base.swap(derived);

        CheckMetaInfoMatches(base);
        CheckMetaInfoMatches(derived);
    }

    {
        BaseWithMeta base;
        DeriverFromBaseWithMeta derived;

        base = derived;

        CheckMetaInfoMatches(base);
        CheckMetaInfoMatches(derived);
    }

    {
        DeriverFromBaseWithMeta derived;
        derived._str = "string";

        BaseWithMeta base(derived);
        UT_AssertIsTrue(base._str == derived._str);

        CheckMetaInfoMatches(base);
        CheckMetaInfoMatches(derived);
    }

    {
        DeriverFromBaseWithMeta derived;
        derived._str = "string";

        DeriverFromBaseWithMeta derived2(derived);
        UT_AssertIsTrue(derived2._str == derived._str);

        CheckMetaInfoMatches(derived);
        CheckMetaInfoMatches(derived2);
    }


    {
        DeriverFromBaseWithMeta derived;
        derived._str = "string";

        bond::bonded<BaseWithMeta> bonded(GetBonded<Reader, Writer, BaseWithMeta>(derived));

        BaseWithMeta data;
        bonded.Deserialize(data);
        UT_AssertIsTrue(data._str == derived._str);

        UT_AssertIsTrue(data._base_meta_full_name_10 == DeriverFromBaseWithMeta::Schema::metadata.qualified_name);
        UT_AssertIsTrue(data._base_meta_name_20 == DeriverFromBaseWithMeta::Schema::metadata.name);
    }

    {
        WithPolymorphic base;
        DerivedFromPolymorphic derived;

        bond::bonded<WithPolymorphic> bonded1(GetBonded<Reader, Writer, WithPolymorphic>(base));
        bond::bonded<WithPolymorphic> bonded2(GetBonded<Reader, Writer, WithPolymorphic>(derived));

        Polymorphic meta;

        bonded1.Deserialize(meta);
        UT_AssertAreEqual(WithPolymorphic::Schema::metadata.qualified_name, meta._bond_meta);

        bonded2.Deserialize(meta);
        UT_AssertAreEqual(DerivedFromPolymorphic::Schema::metadata.qualified_name, meta._bond_meta);
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void BasicTests(const char* name)
{
    UnitTestSuite suite(name);
    
    AddTestCase<TEST_ID(N), MetaTests, Reader, Writer>(suite, "Meta tests");
}


struct SwapTest
{
    template <typename X>
    void operator()(const X&)
    {
        X x1, x2, y1, y2;

        UT_AssertIsTrue(x1 == x2);
        UT_AssertIsTrue(y1 == y2);
        
        x1 = InitRandom<X>();
        y1 = InitRandom<X>();

        UT_AssertIsTrue(x1 != x2);
        UT_AssertIsTrue(y1 != y2);
        
        x2 = x1;
        y2 = y1;

        UT_AssertIsTrue(x1 == x2);
        UT_AssertIsTrue(y1 == y2);

        x2 = InitRandom<X>();
        y2 = InitRandom<X>();

        X x3(x1), x4(x2), y3(y1), y4(y2);

        UT_AssertIsTrue(x1 == x3);
        UT_AssertIsTrue(y1 == y3);

        UT_AssertIsTrue(x2 == x4);
        UT_AssertIsTrue(y2 == y4);

        UT_AssertIsTrue(x1 != x4);
        UT_AssertIsTrue(y1 != y4);

        UT_AssertIsTrue(x3 != x2);
        UT_AssertIsTrue(y3 != y2);

        swap(x1, x2);
        y1.swap(y2);

        UT_AssertIsTrue(x1 == x4);
        UT_AssertIsTrue(y1 == y4);

        UT_AssertIsTrue(x3 == x2);
        UT_AssertIsTrue(y3 == y2);
    }
};


TEST_CASE_BEGIN(SwapTests)
{
    typedef boost::mpl::list<
        SimpleStruct,
        NestedStruct,
        SimpleListsStruct, 
        NestedWithBase1BondedBaseView,
        StructWithNullables,
        BaseWithMeta,
        DeriverFromBaseWithMeta,
        ListWithBase
    > Types;

    boost::mpl::for_each<Types>(SwapTest());
}
TEST_CASE_END


struct CopyMoveTest
{
    template <typename X>
    void operator()(const X&)
    {
        X src = InitRandom<X>();
        
        X x(src);
        UT_AssertIsTrue(x == src);

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
        X y(std::move(x)); 
        UT_AssertIsTrue(y == src);
        UT_AssertIsTrue(moved(x));    
#endif
    }
};


TEST_CASE_BEGIN(CopyMoveTests)
{
    typedef boost::mpl::list<
        SimpleStruct,
        NestedStruct,
        SimpleListsStruct,
        StructWithNullables,
        BaseWithMeta,
        DeriverFromBaseWithMeta,
        ListWithBase
    > Types;

    boost::mpl::for_each<Types>(CopyMoveTest());
}
TEST_CASE_END


template <typename T>
void EnumHelperTest(const std::string& name)
{
    T value;

    UT_AssertIsTrue(ToEnum(value, "x"));
    UT_AssertIsTrue(T::x == value);
    FromString("x", value);
    UT_AssertIsTrue(T::x == value);
    UT_AssertIsTrue(ToString(value) == "x");
    UT_AssertIsTrue(GetValueToNameMap(T()).find(value)->second == "x");
    UT_AssertIsTrue(GetTypeName(value) == name);
    UT_AssertIsTrue(GetTypeName(value, bond::qualified_name) == "unittest." + name);
    UT_AssertThrows(ToString((T)(value+1)), bond::CoreException);
    UT_AssertThrows(FromString("no_such_value", value), bond::CoreException);
}

TEST_CASE_BEGIN(EnumScopeTest)
{
    Color color1, color2;
    Fruit fruit1, fruit2;
    Image image1;

    color1 = Yellow;
    color2 = Color::Orange;

    fruit1 = Grape;
    fruit2 = Fruit::Orange;

    UT_AssertIsTrue(color1 == 2);
    UT_AssertIsTrue(color2 == 1);
    UT_AssertIsTrue(fruit1 == 5);
    UT_AssertIsTrue(fruit2 == 6);    

    UT_AssertIsTrue(image1.color1 == 2);
    UT_AssertIsTrue(image1.color2 == 1);
    UT_AssertIsTrue(image1.fruit1 == 5);
    UT_AssertIsTrue(image1.fruit2 == 6);

    UT_AssertIsTrue(EnumType1::MinInt == std::numeric_limits<int32_t>::min());

    // This test causes ambiguity on Clang - need to invetigate
    /*EnumHelperTest<enum ToEnum>("ToEnum");
    EnumHelperTest<enum ToString>("ToString");
    EnumHelperTest<enum FromString>("FromString");
    EnumHelperTest<enum GetTypeName>("GetTypeName");
    EnumHelperTest<enum GetValueToNameMap>("GetValueToNameMap");*/
    EnumHelperTest<enum SomeEnum>("SomeEnum");
}
TEST_CASE_END


TEST_CASE_BEGIN(SimpleBinaryVersion)
{
    SimpleListsStruct from = InitRandom<SimpleListsStruct>();

    {
        bond::OutputBuffer output_buffer;
    
        // serialize using default version of SimpleBinary reader
        bond::SimpleBinaryWriter<bond::OutputBuffer> output(output_buffer);

        bond::Serialize(from, output);
        bond::InputBuffer input_buffer(output_buffer.GetBuffer());

        {
            // deserialize using version 2 of SimpleBinary reader
            bond::SimpleBinaryReader<bond::InputBuffer> input(input_buffer, 2);
    
            SimpleListsStruct to;
    
            bond::Deserialize(input, to);

            UT_AssertIsTrue(from == to);
        }

        {
            // deserialize using default version of SimpleBinary reader
            bond::SimpleBinaryReader<bond::InputBuffer> input(input_buffer);
    
            SimpleListsStruct to;
    
            bond::Deserialize(input, to);

            UT_AssertIsTrue(from == to);
        }
    }


    {
        bond::OutputBuffer output_buffer;
    
        // serialize using version 2 of SimpleBinary writer
        bond::SimpleBinaryWriter<bond::OutputBuffer> output(output_buffer, 2);

        bond::Serialize(from, output);
        bond::InputBuffer input_buffer(output_buffer.GetBuffer());

        {
            // deserialize using version 2 of SimpleBinary reader
            bond::SimpleBinaryReader<bond::InputBuffer> input(input_buffer, 2);
    
            SimpleListsStruct to;
    
            bond::Deserialize(input, to);

            UT_AssertIsTrue(from == to);
        }

        {
            // deserialize using default version of SimpleBinary reader
            bond::SimpleBinaryReader<bond::InputBuffer> input(input_buffer);
    
            SimpleListsStruct to;
    
            bond::Deserialize(input, to);

            UT_AssertIsTrue(from == to);
        }
    }

    {
        bond::OutputBuffer output_buffer;
    
        // serialize using version 1 of SimpleBinary writer
        bond::SimpleBinaryWriter<bond::OutputBuffer> output(output_buffer, 1);

        bond::Serialize(from, output);
        bond::InputBuffer input_buffer(output_buffer.GetBuffer());

        {
            // deserialize using version 1 of SimpleBinary reader
            bond::SimpleBinaryReader<bond::InputBuffer> input(input_buffer, 1);
    
            SimpleListsStruct to;
    
            bond::Deserialize(input, to);

            UT_AssertIsTrue(from == to);
        }
    }
}
TEST_CASE_END


void BasicTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        BasicTests<
            0xb01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Basic functionality tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        BasicTests<
            0xb02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Basic functionality tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        BasicTests<
            0xb03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Basic functionality tests for FastBinary");
    );

    UnitTestSuite suite("Protocol independent tests");
    
    AddTestCase<TEST_ID(0xb05), SwapTests>(suite, "Swap tests");
    AddTestCase<TEST_ID(0xb06), SimpleBinaryVersion>(suite, "Simple Protocol version");
    AddTestCase<TEST_ID(0xb07), CopyMoveTests>(suite, "Copy and Move tests");
    AddTestCase<TEST_ID(0xb08), EnumScopeTest>(suite, "Enum scope tests");
}

