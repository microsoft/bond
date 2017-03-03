#include "precompiled.h"
#include "bonded_tests.h"
#include <bond/core/validate.h>

template <typename T>
void BondedVoid(const bond::bonded<void>& bonded, const T& expected)
{
    T value;

    bonded.Deserialize(value);

    UT_AssertIsTrue(Equal(expected, value));

    CopyAndMove(value);

    auto value2 = bonded.Deserialize<T>();
    UT_AssertIsTrue(Equal(expected, value2));
}


template <typename T>
void BondedTyped(const bond::bonded<T>& bonded, const T& expected)
{
    T value;

    bonded.Deserialize(value);

    UT_AssertIsTrue(Equal(expected, value));

    CopyAndMove(value);

    auto value2 = bonded.Deserialize();
    UT_AssertIsTrue(Equal(expected, value2));
}


template <typename T1, typename T2>
void BondedCast(const bond::bonded<T1>& bonded, const T2& expected)
{
    T2 value;

    bonded.Deserialize(value);

    UT_AssertIsTrue(Equal(expected, value));

    auto value2 = bonded.template Deserialize<T2>();
    UT_AssertIsTrue(Equal(expected, value2));
}


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(BondedCasts)
{
    T value = InitRandom<T>();

    {
        bond::bonded<T> bonded(Serialize<Reader, Writer>(value, Reader::version));

        // Deserialize T from bonded<T>
        BondedTyped(bonded, value);

        // Cast from bonded<T>
        BondedCast(bonded, value);

        // Cast bonded<T> to bonded<void>
        BondedVoid(bonded, value);
    }

    {
        bond::bonded<bond::SchemaDef> bondedSchema(Serialize<Reader, Writer>(bond::GetRuntimeSchema<T>().GetSchema()));

        boost::shared_ptr<bond::SchemaDef> schema(new bond::SchemaDef);

        bondedSchema.Deserialize(*schema);

        bond::bonded<void> bondedVoid(Serialize<Reader, Writer>(value, Reader::version), schema);

        // Deserialize from bonded<void>
        BondedVoid(bondedVoid, value);

        bond::bonded<T> bonded(bondedVoid);

        // cast bonded<void> to bonded<T>
        BondedTyped(bonded, value);
    }
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(BondedConstructors)
{
    {
        SimpleStruct simple = InitRandom<SimpleStruct>();
        SimpleStruct original(simple);

        {
            NestedStruct1BondedView from, from2;

            from.s = bond::bonded<SimpleStruct>(simple);

            from2 = from;

            simple.m_int8++;

            UT_AssertIsTrue(from == from2);

            NestedStruct1 to, to2;

            SerializeDeserialize<Reader, Writer>(from, to);
            SerializeDeserialize<Reader, Writer>(from2, to2, Reader::version);

            UT_AssertIsTrue(simple != original);
            UT_Equal(to.s, original);
            UT_Equal(to, to2);
        }

        {
            NestedStruct1BondedView from, from2;

            from.s = bond::bonded<SimpleStruct>(boost::cref(simple));

            from2 = from;

            simple.m_int8++;

            UT_AssertIsTrue(from == from2);

            NestedStruct1 to, to2;

            SerializeDeserialize<Reader, Writer>(from, to);
            SerializeDeserialize<Reader, Writer>(from2, to2, Reader::version);

            UT_AssertIsTrue(simple != original);
            UT_Equal(to.s, simple);
            UT_Equal(to, to2);
        }

        {
            NestedStruct1BondedView         from, from2;
            boost::shared_ptr<SimpleStruct> simple_ptr(new SimpleStruct(simple));

            from.s = bond::bonded<SimpleStruct>(simple_ptr);

            from2 = from;

            simple.m_int8++;
            simple_ptr->m_int16++;

            UT_AssertIsTrue(from == from2);

            NestedStruct1 to, to2;

            SerializeDeserialize<Reader, Writer>(from, to);
            SerializeDeserialize<Reader, Writer>(from2, to2, Reader::version);

            UT_AssertIsTrue(*simple_ptr != original);
            UT_AssertIsTrue(*simple_ptr != simple);
            UT_Equal(to.s, *simple_ptr);
            UT_Equal(to, to2);
        }
    }

    for (unsigned i = 0; i < 3; ++i)
    {
        NestedWithBase1BondedBaseView   from, from2;
        StructWithBase                  simple = InitRandom<StructWithBase>();
        StructWithBase                  original(simple); 
        bond::bonded<SimpleBase>        bonded_simple_base;
        bond::bonded<SimpleStruct>      bonded_simple_struct;
        
        if (i == 0)
        {
            bonded_simple_base = bond::bonded<SimpleBase>(simple);
            bonded_simple_struct = bond::bonded<SimpleStruct>(simple);

            simple.m_int8++;
        }
        else if (i == 1)
        {
            bonded_simple_base = bond::bonded<SimpleBase>(boost::cref(simple));
            bonded_simple_struct = bond::bonded<SimpleStruct>(boost::cref(simple));
        }
        else if (i == 2)
        {
            bonded_simple_base = bond::bonded<SimpleBase>(
                                        boost::shared_ptr<StructWithBase>(new StructWithBase(simple)));
            bonded_simple_struct = bond::bonded<SimpleStruct>(
                                        boost::shared_ptr<StructWithBase>(new StructWithBase(simple)));
        }

        // StructWithBase derives from SimpleBase which derives from SimpleStruct

        // explicit down-cast from bonded<SimpleStruct> to bonded<SimpleBase>
        from.s1 = bond::bonded<SimpleBase>(bonded_simple_struct);

        // implicit up-cast from bonded<SimpleBase> to bonded<SimpleStruct>
        from.s2 = bonded_simple_base;

        from2 = from;

        UT_AssertIsTrue(from == from2);

        NestedWithBase1BaseView to, to2;

        SerializeDeserialize<Reader, Writer>(from, to);
        SerializeDeserialize<Reader, Writer>(from2, to2, Reader::version);

        UT_Equal(static_cast<SimpleBase&>(to.s1), static_cast<SimpleBase&>(original));
        UT_Equal(static_cast<SimpleStruct&>(to.s2), static_cast<SimpleStruct&>(original));
        UT_Equal(to, to2);
    }
}
TEST_CASE_END


template <typename Reader, typename Writer>
void BondedSerializeImpl(uint16_t version)
{
    unittest::NestedWithBase1BondedBaseView from, to;

    from = InitRandom<unittest::NestedWithBase1BondedBaseView>();

    SerializeDeserialize<Reader, Writer>(from, to, version);

    SimpleBase      from_s1, to_s1;
    SimpleStruct    from_s2, to_s2;

    from.s1.Deserialize(from_s1);
    from.s2.Deserialize(from_s2);

    to.s1.Deserialize(to_s1);
    to.s2.Deserialize(to_s2);

    UT_Equal(to_s1, from_s1);
    UT_Equal(to_s2, from_s2);
}


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(BondedSerialize)
{
    BondedSerializeImpl<Reader, Writer>(Reader::version);
    BondedSerializeImpl<Reader, Writer>(bond::v1);
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T1, typename T2>
TEST_CASE_BEGIN(MarshaledBonded)
{
    typedef BondStruct<T1> From;
    typedef BondStruct<T2> To;

    // 1. Serialize From
    From from = InitRandom<From>();
    Reader reader = Serialize<Reader, Writer>(from);

    // 1.a. Deserialize From
    {
        From to;
        UT_AssertIsTrue(bond::Validate(bond::GetRuntimeSchema<From>(), bond::GetRuntimeSchema<From>()));
        bond::Deserialize(reader, to);
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        From to;
        bond::Deserialize(reader, to, bond::GetRuntimeSchema<From>());
        UT_AssertIsTrue(Equal(from, to));
    }

    // 1.b. Deserialize To
    {
        To to;
        UT_AssertIsFalse(bond::Validate(bond::GetRuntimeSchema<From>(), bond::GetRuntimeSchema<To>()));
        bond::Deserialize(reader, to, bond::GetRuntimeSchema<From>());
        UT_AssertIsTrue(Equal(from.field, to.field));
    }
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T1, typename T2>
TEST_CASE_BEGIN(MarshaledBondedDerived)
{
    typedef BondStruct<bond::bonded<T1> > From;
    typedef BondStruct<bond::bonded<T2> > To;
    typedef BondStruct<T2> To2;

    // 1. Serialize bonded<T1> with T2
    T2 t2 = InitRandom<T2>();
    From from;
    from.field = bond::bonded<T1>(bond::bonded<void>(GetBonded<Reader, Writer, T2>(t2)));
    Reader reader = Serialize<Reader, Writer>(from);

    // 1.a. Deserialize bonded<T2>
    {
        To to;
        UT_AssertIsTrue(bond::Validate(bond::GetRuntimeSchema<From>(), bond::GetRuntimeSchema<To>()));
        bond::Deserialize(reader, to);
        UT_AssertIsTrue(Equal(t2, to.field));
    }

    {
        To to;
        bond::Deserialize(reader, to, bond::GetRuntimeSchema<From>());
        UT_AssertIsTrue(Equal(t2, to.field));
    }

    // 1.b. Deserialize T2
    {
        To2 to;
        // Saying that payload is of type To is a bit of a hack that allows us
        // to deserialize into instance of T2 directly instead of going via bonded<T2>
        UT_AssertIsFalse(bond::Validate(bond::GetRuntimeSchema<To>(), bond::GetRuntimeSchema<To2>()));
        bond::Deserialize(reader, to, bond::GetRuntimeSchema<To>());
        UT_AssertIsTrue(Equal(t2, to.field));
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void BondedTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), BondedConstructors, Reader, Writer>(suite, "bonded constructors");
    AddTestCase<TEST_ID(N), BondedCasts, Reader, Writer, unittest::NestedStruct>(suite, "bonded casts");

    TEST_SIMPLE_PROTOCOL(
        // Uses Simple protocol for random initialization of struct with bonded<T>
        AddTestCase<TEST_ID(N), BondedSerialize, Reader, Writer>(suite, "bonded serialization");
    );
}


template <uint16_t N, typename Reader, typename Writer>
void MarshaledBondedTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        MarshaledBonded, Reader, Writer, SimpleStruct, bond::bonded<SimpleStruct> >(suite, "T to bonded");

    AddTestCase<TEST_ID(N),
        MarshaledBonded, Reader, Writer, bond::bonded<SimpleStruct>, SimpleStruct>(suite, "bonded to T");

    AddTestCase<TEST_ID(N),
        MarshaledBondedDerived, Reader, Writer, SimpleStruct, StructWithBase>(suite, "bonded base to bonded derived");
}


void BondedTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        BondedTests<
            0x201,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("bonded tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        BondedTests<
            0x202,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("bonded tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        BondedTests<
            0x203,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("bonded tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        BondedTests<
            0x204,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("bonded tests for Simple JSON");
    );

    TEST_SIMPLE_PROTOCOL(
        MarshaledBondedTests<
            0x206,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Marshaled bonded tests for SimpleBinary");
    );
}


bool init_unit_test()
{
    BondedTest::Initialize();
    return true;
}
