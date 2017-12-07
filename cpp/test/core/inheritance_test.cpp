#include "precompiled.h"
#include "serialization_test.h"


template <typename Protocols>
bool Compare(const ListWithBase& left, const ListOfBase& right)
{
    return Equal<Protocols>(left.l1, right.l1)
        && Equal<Protocols>(left.vl2, right.vl2)
        && Equal<Protocols>(left.v4, right.v4);
}

template <typename Reader, typename Writer>
typename boost::enable_if<bond::uses_dynamic_parser<Reader> >::type
DeserializeBaseToDerived()
{
    ListOfBondedBase obj;
    GetBonded<Reader, Writer, ListOfBondedBase>(InitRandom<ListOfBondedBase>()).Deserialize(obj);

    for (const auto& base : obj.l)
    {
        bond::bonded<StructWithBase> derived_bonded(base);
        StructWithBase derived;
        BOOST_CHECK_THROW(derived_bonded.Deserialize(derived), bond::CoreException);
        BOOST_CHECK_THROW(bond::bonded<void>(derived_bonded).Deserialize(derived), bond::CoreException);
    }
}

template <typename Reader, typename Writer>
typename boost::disable_if<bond::uses_dynamic_parser<Reader> >::type
DeserializeBaseToDerived()
{}

template <typename Reader, typename Writer>
TEST_CASE_BEGIN(BaseToDerivedDeserializationTest)
{
    DeserializeBaseToDerived<Reader, Writer>();
}
TEST_CASE_END

template <uint16_t N, typename Reader, typename Writer>
void InheritanceTests(const char* name)
{
    UnitTestSuite suite(name);

    // Deserialize the same type as was serialized
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, StructWithBase>(suite, "Simple struct");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, NestedWithBase>(suite, "Nested struct");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, ListWithBase>(suite, "Containers");

    // Deserialize a different version of the type that was serialized
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, StructWithBase, StructWithBaseView>(suite, "Simple struct, partial view");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, NestedWithBase, NestedWithBaseView>(suite, "Nested struct, partial view");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, ListWithBase, ListWithBaseView>(suite, "Containers, partial view");

    // Deserialize base class only
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, StructWithBase, SimpleStruct>(suite, "Simple struct, base view");

    // Deserialize partial hierarchy
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, StructWithBase, SimpleBase>(suite, "Simple struct, partial view");

    // Deserialize base class only via partial schema
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping3, Reader, Writer, StructWithBase, SimpleStruct, SimpleBase>(suite, "Base via partial hierarchy");

    // Deserialize as containers of base/partial hierarchy
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, ListWithBase, ListOfBase>(suite, "Containers, partial hierarchy");

    AddTestCase<TEST_ID(N),
        BaseToDerivedDeserializationTest, Reader, Writer>(suite, "Base to derived deserialization");
}


void InheritanceTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        InheritanceTests<
            0x301,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Inheritance tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        InheritanceTests<
            0x302,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Inheritance tests for CompactBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        InheritanceTests<
            0x303,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Inheritance tests for Simple JSON");
    );

    TEST_FAST_BINARY_PROTOCOL(
        InheritanceTests<
            0x304,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Inheritance tests for FastBinary");
    );
}

bool init_unit_test()
{
    InheritanceTestsInit();
    return true;
}
