#include "precompiled.h"
#include "apply_tests.h"
#include "apply_test_apply.h"   // Note that we don't want to include apply_test_reflection.h so that
                                // we only see pre-generated Apply overloads.

void Init(unittest::apply::Struct& obj)
{
    obj = InitRandom<unittest::apply::Struct>();
}


void Init(unittest::apply::Derived& obj)
{
    obj = InitRandom<unittest::apply::Derived>();
}


template <typename Reader, typename Writer, typename X>
void Marshal(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        bond::Marshal<X, Writer>, obj, _1));

    bond::InputBuffer input = output.GetBuffer();

    Unmarshal(input, obj2);

    UT_AssertIsTrue(obj == obj2);
}


template <typename Reader, typename Writer, typename X>
void Serialize(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        bond::Serialize<X, Writer>, obj, _1));

    bond::InputBuffer input = output.GetBuffer();
    Reader reader(Factory<Reader>::Create(input, version));

    Deserialize(reader, obj2);

    UT_AssertIsTrue(obj == obj2);
}


template <typename Transform, typename T>
void CallApply(const Transform& transform, const T& value)
{
    Apply(transform, value);
}

template <typename Reader, typename Writer, typename X>
void Apply(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        CallApply<bond::Serializer<Writer>, X>, boost::bind(bond::SerializeTo<Writer>, _1), obj));

    bond::InputBuffer input = output.GetBuffer();
    Reader reader(Factory<Reader>::Create(input, version));
    bond::bonded<X> bonded(reader);

    Apply(bond::To<X>(obj2), bonded);

    UT_AssertIsTrue(obj == obj2);
}


template <typename Reader, typename Writer, typename X>
typename boost::enable_if<bond::uses_static_parser<Reader> >::type
Bonded(uint16_t = bond::v1)
{
    // apply_test_reflection.h is necessary for pass-through of untagged
    // protocols such as Simple even when using *_apply because struct skipping 
    // depends on compile-time schema defined in _reflection.h.
}

template <typename Reader, typename Writer, typename X>
typename boost::disable_if<bond::uses_static_parser<Reader> >::type
Bonded(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        bond::Serialize<X, Writer>, obj, _1));

    bond::InputBuffer input = output.GetBuffer();
    Reader reader(Factory<Reader>::Create(input, version));
    bond::bonded<X> bonded(reader);

    bond::OutputBuffer output2;
    Factory<Writer>::Call(output2, version, boost::bind(
        &bond::bonded<X>::template Serialize<Writer>, bonded, _1));

    bond::InputBuffer input2 = output2.GetBuffer();
    Reader reader2(Factory<Reader>::Create(input2, version));
    bond::bonded<X> bonded2(reader2);
    
    bonded2.Deserialize(obj2);

    UT_AssertIsTrue(obj == obj2);
}


template <typename Reader, typename Writer>
struct Tests
{
    template <typename X>
    void operator()(const X&)
    {
        Marshal<Reader, Writer, X>();
        Marshal<Reader, Writer, X>(Reader::version);
        
        Serialize<Reader, Writer, X>();
        Serialize<Reader, Writer, X>(Reader::version);

        Apply<Reader, Writer, X>();
        Apply<Reader, Writer, X>(Reader::version);

        Bonded<Reader, Writer, X>();
        Bonded<Reader, Writer, X>(Reader::version);

        bond::RuntimeSchema schema = bond::GetRuntimeSchema<X>();
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(AllTests)
{
    typedef boost::mpl::list<
        unittest::apply::Struct,
        unittest::apply::Derived
    > Types;

    boost::mpl::for_each<Types>(Tests<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void ApplyTests(const char* name)
{
    UnitTestSuite suite(name);
    
    AddTestCase<TEST_ID(N), AllTests, Reader, Writer>(suite, "Use generated *_apply.cpp");
}


void ApplyTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        ApplyTests<
            0x1601,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Apply tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        ApplyTests<
            0x1602,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Apply tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        ApplyTests<
            0x1603,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Apply tests for FastBinary");
    );
}


bool init_unit_test()
{
    ApplyTest::Initialize();
    return true;
}

