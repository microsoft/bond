#include "precompiled.h"
#include "serialization_test.h"
#include "numeric_conversions.h"


template <typename Reader, typename Writer>
struct NumericConverter1
{
    template <typename Num1>
    void operator()(const Num1&)
    {
        boost::mpl::for_each<typename compatible_types<Num1>::type>(NumericConverter2<Reader, Writer, Num1>());
        boost::mpl::for_each<typename incompatible_types<Num1>::type>(NumericConverterFail<Reader, Writer, Num1>());
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(NumericConversions)
{
    boost::mpl::for_each<NumericTypes>(NumericConverter1<Reader, Writer>());
}
TEST_CASE_END


template <typename Reader, typename Writer, typename From, typename To>
void ContainerFail(From& from)
{
    UT_AssertIsFalse(ValidateCompatible<From, To>());

    bond::bonded<From> bonded(GetBonded<Reader, Writer, From>(from));
    bond::bonded<void> bonded_void(bonded);

    To to;

    bonded.Deserialize(to);
    UT_AssertIsTrue(to.field.empty());

    bonded_void.Deserialize(to);
    UT_AssertIsTrue(to.field.empty());    
}


template <typename Reader, typename Writer, typename Num1>
struct NonmatchingList
{
    template <typename Num2>
    void operator()(const Num2&)
    {
        typedef BondStruct<std::list<Num1> > From;
        typedef BondStruct<std::vector<Num2> > To;

        From from;

        from.field.push_back(static_cast<Num1>(0));

        ContainerFail<Reader, Writer, From, To>(from);
    }
};


template <typename Reader, typename Writer, typename Num1>
struct NonmatchingNullable
{
    template <typename Num2>
    void operator()(const Num2&)
    {
        typedef BondStruct<bond::nullable<Num1> > From;
        typedef BondStruct<bond::nullable<Num2> > To;

        From from;

        from.field.set() = static_cast<Num1>(0);

        ContainerFail<Reader, Writer, From, To>(from);
    }
};


template <typename Reader, typename Writer, typename Num1>
struct NonmatchingSet
{
    template <typename Num2>
    void operator()(const Num2&)
    {
        typedef BondStruct<std::set<Num1> > From;
        typedef BondStruct<std::set<Num2> > To;

        From from;

        from.field.insert(static_cast<Num1>(0));

        ContainerFail<Reader, Writer, From, To>(from);
    }
};


template <typename Reader, typename Writer, typename Num1>
struct NonmatchingMap
{
    template <typename Num2>
    void operator()(const Num2&)
    {
        typedef BondStruct<std::map<Num1, Num1> > From;
        
        From from;

        from.field[static_cast<Num1>(0)] = static_cast<Num1>(0);
        
        {
            typedef BondStruct<std::map<Num2, Num1> > To;

            ContainerFail<Reader, Writer, From, To>(from);
        }

        {
            typedef BondStruct<std::map<Num1, Num2> > To;

            ContainerFail<Reader, Writer, From, To>(from);
        }
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(NonmatchingContainers)
{
    boost::mpl::for_each<incompatible_types<bool>::type>(NonmatchingList<Reader, Writer, bool>());
    boost::mpl::for_each<incompatible_types<double>::type>(NonmatchingNullable<Reader, Writer, double>());
    boost::mpl::for_each<incompatible_types<int64_t>::type>(NonmatchingSet<Reader, Writer, int64_t>());
    boost::mpl::for_each<incompatible_types<uint64_t>::type>(NonmatchingMap<Reader, Writer, uint64_t>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void NumericConversionTests(const char* name)
{
    UnitTestSuite suite(name);

    // For every numeric type Num1, serialize a structure with a single field 
    // of that type and deserialize it as a structure with a single field for 
    // every other numeric type Num2. 
    // The test succeeds if the value of the field in the deserialized structure
    // is equal to a static_cast<Num2> of the value in serialized structure.
    AddTestCase<TEST_ID(N), NumericConversions, Reader, Writer>(suite, "Numeric conversions");

    AddTestCase<TEST_ID(N), NonmatchingContainers, Reader, Writer>(suite, "Nonmatching containers");
}


void NumericConversionTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        NumericConversionTests<
            0x601,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Numeric conversion tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        NumericConversionTests<
            0x602,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Numeric conversion tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        NumericConversionTests<
            0x603,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Numeric conversion tests for FastBinary");
    );
}

bool init_unit_test()
{
    NumericConversionTestsInit();
    return true;
}

