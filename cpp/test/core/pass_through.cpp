#include "precompiled.h"
#include "serialization_test.h"
#include "untagged_protocol.h"


// These Compare functions are quite expensive to compile; they are made into function templates
// so that the code is not generated when building a single unit test
template <typename Protocols, typename NestedStruct>
bool Compare(const NestedStruct& left, const NestedStructBondedView& right)
{
    return Compare<Protocols>(left.m_bool, right.m_bool)
        && Compare<Protocols>(left.m_int8, left.m_int8)
        && Equal<Protocols>(left.n3, right.n3)
        && Equal<Protocols>(left.n2, right.n2)
        && Equal<Protocols>(left.n1, right.n1);
}


template <typename Protocols>
bool Compare(const NestedStruct1& left, const NestedStruct1BondedView& right)
{
    return Equal<Protocols>(left.s, right.s);
}


template <typename Protocols, typename NestedListsStruct>
bool Compare(const NestedListsStruct& left, const NestedListsStructBondedView& right)
{
    return Equal<Protocols>(left.lSLS, right.lSLS)
        && Equal<Protocols>(left.vlSLS, right.vlSLS)
        && Equal<Protocols>(left.vvNS, right.vvNS)
        && Equal<Protocols>(left.m64ls, right.m64ls);
}


template <typename Protocols, typename NestedWithBase1>
bool Compare(const NestedWithBase1& left, const NestedWithBase1BondedBaseView& right)
{
    return Equal<Protocols>(left.s1, right.s1)
        && Equal<Protocols>(left.s2, right.s2);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename T, typename Through, typename BondedType>
void PassThrough(uint16_t version = bond::v1)
{
    T from = InitRandom<T>();

    bond::bonded<BondedType> bonded_from(GetBonded<Reader1, Writer1, T>(from, version));

    Through through;

    bonded_from.Deserialize(through);

    typename Writer2::Buffer output_buffer;
    Writer2 output(output_buffer);

    Serialize(through, output);

    typename Reader2::Buffer input_buffer(output_buffer.GetBuffer());
    Reader2 input(input_buffer);
    bond::bonded<T> bonded_to(input);

    T to = InitRandom<T>();

    bonded_to.Deserialize(to);

    UT_Compare(to, from);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To, typename BondedType, typename Protocols>
void Transcoding(uint16_t version1 = bond::v1, uint16_t version2 = bond::v1)
{
    From from = InitRandom<From, Protocols>();

    // Compile-time schema
    {
        bond::bonded<BondedType> bonded_from(GetBonded<Reader1, Writer1, BondedType, Protocols>(from, version1));

        bond::bonded<To> bonded_to(GetBonded<Reader2, Writer2, To, Protocols>(bonded_from, version2));

        To to = InitRandom<To, Protocols>();

        bonded_to.template Deserialize<Protocols>(to);

        UT_Equal_P(from, to, Protocols);
    }

    // Run-time schema
    {
        bond::bonded<void> bonded_from(GetBonded<Reader1, Writer1,
                                       typename boost::mpl::if_<bond::has_schema<BondedType>,
                                                                BondedType,
                                                                From>::type, Protocols>(from, version1));

        bond::bonded<To> bonded_to(GetBonded<Reader2, Writer2, To, Protocols>(bonded_from, version2));

        To to = InitRandom<To, Protocols>();

        bonded_to.template Deserialize<Protocols>(to);

        UT_Equal_P(from, to, Protocols);
    }
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To, typename BondedType>
void BondedSerialize(uint16_t version = bond::v1)
{
    From from = InitRandom<From>();

    bond::bonded<BondedType> bonded_from(GetBonded<Reader1, Writer1, BondedType>(from, version));

    To pass_through;

    bonded_from.Deserialize(pass_through);

    // Compile-time schema
    {
        bond::bonded<To> bonded_pass_through(GetBonded<Reader2, Writer2, To>(pass_through, version));

        To to = InitRandom<To>();

        bonded_pass_through.Deserialize(to);

        UT_Compare(from, to);
    }

    // Run-time schema
    {
        bond::bonded<void> bonded_pass_through(GetBonded<Reader2, Writer2, To>(pass_through, version));

        To to = InitRandom<To>();

        bonded_pass_through.Deserialize(to);

        UT_Compare(from, to);
    }
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To>
typename boost::enable_if_c<bond::uses_dynamic_parser<Reader1>::value && bond::uses_dynamic_parser<Reader2>::value>::type
AllTypesBondedSerialize()
{
    BondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To, From>();
    BondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To, To>();
    BondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To, From>(Reader1::version);
    BondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To, To>(Reader1::version);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To>
typename boost::enable_if_c<bond::uses_static_parser<Reader1>::value || bond::uses_static_parser<Reader2>::value>::type
AllTypesBondedSerialize()
{
    BondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To, From>();
    BondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To, From>(Reader1::version);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To>
void AllProtoBondedSerialize()
{
    AllTypesBondedSerialize<Reader1, Writer1, Reader1, Writer1, From, To>();
    AllTypesBondedSerialize<Reader2, Writer2, Reader2, Writer2, From, To>();
    AllTypesBondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To>();
    AllTypesBondedSerialize<Reader2, Writer2, Reader1, Writer1, From, To>();
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
typename boost::disable_if_c<bond::uses_static_parser<Reader1>::value || bond::uses_static_parser<Reader2>::value || std::is_same<Reader2, bond::SimpleJsonReader<typename Reader2::Buffer> >::value>::type
AllTypesTranscoding()
{
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>();
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, ForwardDeclarationOnly, Protocols>();
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>(Reader1::version);
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>(bond::v1, Reader2::version);
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>(Reader1::version, Reader2::version);
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, ForwardDeclarationOnly, Protocols>(Reader1::version);
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, ForwardDeclarationOnly, Protocols>(Reader1::version, Reader2::version);
}


// Simple JSON flattens inheritance hierarchy and thus doesn't always supprot transcoding with w/o schema becase
// ordinal used for fields in JSON when names are not available can collide.
template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<bond::uses_static_parser<Reader1>::value || bond::uses_static_parser<Reader2>::value || std::is_same<Reader2, bond::SimpleJsonReader<typename Reader2::Buffer> >::value>::type
AllTypesTranscoding()
{
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>();
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>(Reader1::version);
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>(bond::v1, Reader2::version);
    Transcoding<Reader1, Writer1, Reader2, Writer2, From, To, From, Protocols>(Reader1::version, Reader2::version);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To>
void AllProtoTranscoding()
{
    AllTypesTranscoding<Reader1, Writer1, Reader1, Writer1, From, To>();
    AllTypesTranscoding<Reader2, Writer2, Reader2, Writer2, From, To>();
    AllTypesTranscoding<Reader1, Writer1, Reader2, Writer2, From, To>();
    AllTypesTranscoding<Reader2, Writer2, Reader1, Writer1, From, To>();
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename T, typename Through>
typename boost::enable_if_c<bond::uses_dynamic_parser<Reader1>::value && bond::uses_dynamic_parser<Reader2>::value>::type
AllTypesPassThrough()
{
    PassThrough<Reader1, Writer1, Reader2, Writer2, T, Through, T>();
    PassThrough<Reader1, Writer1, Reader2, Writer2, T, Through, void>();
    PassThrough<Reader1, Writer1, Reader2, Writer2, T, Through, T>(Reader1::version);
    PassThrough<Reader1, Writer1, Reader2, Writer2, T, Through, void>(Reader1::version);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename T, typename Through>
typename boost::enable_if_c<bond::uses_static_parser<Reader1>::value && bond::uses_dynamic_parser<Reader2>::value>::type
AllTypesPassThrough()
{
    PassThrough<Reader1, Writer1, Reader2, Writer2, T, Through, void>();
    PassThrough<Reader1, Writer1, Reader2, Writer2, T, Through, void>(Reader1::version);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename T, typename Through>
typename boost::enable_if<bond::uses_static_parser<Reader2> >::type
AllTypesPassThrough()
{
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename T, typename Through>
void AllProtoPassThrough()
{
    AllTypesPassThrough<Reader1, Writer1, Reader1, Writer1, T, Through>();
    AllTypesPassThrough<Reader2, Writer2, Reader2, Writer2, T, Through>();
    AllTypesPassThrough<Reader1, Writer1, Reader2, Writer2, T, Through>();
    AllTypesPassThrough<Reader2, Writer2, Reader1, Writer1, T, Through>();
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To>
TEST_CASE_BEGIN(AllBondedSerialize)
{
    AllProtoBondedSerialize<Reader1, Writer1, Reader2, Writer2, From, To>();
}
TEST_CASE_END


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To>
TEST_CASE_BEGIN(AllTranscoding)
{
    AllProtoTranscoding<Reader1, Writer1, Reader2, Writer2, From, To>();
}
TEST_CASE_END


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename From, typename To, typename Protocols>
TEST_CASE_BEGIN(TranscodingTest)
{
    AllTypesTranscoding<Reader1, Writer1, Reader2, Writer2, From, To, Protocols>();
}
TEST_CASE_END


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename T, typename Through>
TEST_CASE_BEGIN(AllPassThrough)
{
    AllProtoPassThrough<Reader1, Writer1, Reader2, Writer2, T, Through>();
}
TEST_CASE_END


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2>
struct TranscodingType
{
    template <typename X>
    void operator()(const X&)
    {
        typedef BondStruct<X> T;

        AllProtoTranscoding<Reader1, Writer1, Reader2, Writer2, T, T>();
    }
};


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2>
TEST_CASE_BEGIN(BasicTypesTranscoding)
{
    boost::mpl::for_each<BasicTypes>(TranscodingType<Reader1, Writer1, Reader2, Writer2>());

    // deserialization of empty bonded<T>
    NestedStruct1           n1;
    NestedStruct1BondedView n2;

    UT_Compare(n1, n2);
}
TEST_CASE_END


namespace bond
{
    namespace detail
    {
        // Supress assert on optional fields being set to default values
        // for OptionalContainers and StructWithDefaults used in
        // DefaultValuesTranscoding test case.
        template <>
        class OptionalDefault<OptionalContainers>
        {
        public:
            OptionalDefault(const OptionalContainers&)
            {}

            operator bool()
            {
                return true;
            }
        };

        template <>
        class OptionalDefault<StructWithDefaults>
        {
        public:
            OptionalDefault(const StructWithDefaults&)
            {}

            operator bool()
            {
                return true;
            }
        };
    }
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename Bonded, typename Protocols, typename T>
void DefaultValuesTranscodingTest(T to, uint16_t version1 = bond::v1, uint16_t version2 = bond::v1)
{
    T from;

    bond::bonded<Bonded> bonded1(GetBonded<Reader1, Writer1, T, Protocols>(from, version1));

    // OutputBuffer is not really designed to allow writing before current pointer
    // which is required for our test untagged protocol. However it works if
    // memory for OutputBuffer is preallocated.
    typename Writer2::Buffer output_buffer(4096);

    Factory<Writer2>::Call(output_buffer, version2, boost::bind(
        &bond::bonded<Bonded>::template Serialize<Protocols, Writer2>, bonded1, boost::placeholders::_1));

    typename Reader2::Buffer input_buffer(output_buffer.GetBuffer());
    Reader2 input = Factory<Reader2>::Create(input_buffer, version2);

    bond::Deserialize<Protocols>(input, to);

    UT_Compare_P(from, to, Protocols);
}


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename Protocols>
TEST_CASE_BEGIN(DefaultValuesTranscoding)
{
    typedef OptionalContainers T;

    T init;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127) // C4127: conditional expression is constant
#endif
    if (bond::uses_dynamic_parser<Reader1>::value && !bond::may_omit_fields<Writer2>::value)
#ifdef _MSC_VER
#pragma warning(pop)
#endif
    {
        // transcoding from tagged protocol using runtime schema fills-in default values
        // so we can use random object as initial value of 'to'.
        init = InitRandom<T>();
    }

    DefaultValuesTranscodingTest<Reader1, Writer1, Reader2, Writer2, void, Protocols>(init);
    DefaultValuesTranscodingTest<Reader1, Writer1, Reader2, Writer2, void, Protocols>(init, bond::v1, Reader2::version);
    DefaultValuesTranscodingTest<Reader1, Writer1, Reader2, Writer2, void, Protocols>(init, Reader1::version, Reader2::version);
}
TEST_CASE_END


template <typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename Protocols>
TEST_CASE_BEGIN(OmittedDefaultValuesTranscoding)
{
    typedef OptionalNothing T;

    T init;

    DefaultValuesTranscodingTest<Reader1, Writer1, Reader2, Writer2, T, Protocols>(init);
    DefaultValuesTranscodingTest<Reader1, Writer1, Reader2, Writer2, T, Protocols>(init, bond::v1, Reader2::version);
    DefaultValuesTranscodingTest<Reader1, Writer1, Reader2, Writer2, T, Protocols>(init, Reader1::version, Reader2::version);
}
TEST_CASE_END


template <uint16_t N, typename Reader1, typename Writer1, typename Reader2, typename Writer2>
void PassThroughTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        AllBondedSerialize, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedStruct, unittest::NestedStructBondedView>(suite, "Serialize bonded T");

    AddTestCase<TEST_ID(N),
        AllPassThrough, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedWithBase1, unittest::NestedWithBase1BondedBaseView>(suite, "Pass-through base");

    AddTestCase<TEST_ID(N),
        AllPassThrough, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedListsStruct, unittest::NestedListsStructPassThrough>(suite, "Pass-through containers");

    AddTestCase<TEST_ID(N),
        AllBondedSerialize, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedListsStruct, unittest::NestedListsStructBondedView>(suite, "Serialize list of bonded");

    AddTestCase<TEST_ID(N),
        AllTranscoding, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedListsStruct, unittest::NestedListsStruct>(suite, "Transcoding containers");

    AddTestCase<TEST_ID(N),
        AllTranscoding, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedWithBase, unittest::NestedWithBase>(suite, "Transcoding with inheritance");

    AddTestCase<TEST_ID(N),
        BasicTypesTranscoding, Reader1, Writer1, Reader2, Writer2>(suite, "Transcoding basic types");

    AddTestCase<TEST_ID(N),
        DefaultValuesTranscoding, Reader1, Writer1, Reader2, Writer2, bond::BuiltInProtocols>(suite, "Transcoding default values");
}


template <uint16_t N, typename Reader, typename Writer>
void PassThroughTests(const char* name)
{
    PassThroughTests<N, Reader, Writer, Reader, Writer>(name);
}


template <uint16_t N, typename Reader1, typename Writer1, typename Reader2, typename Writer2, typename Protocols = bond::BuiltInProtocols>
void TranscodingTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        TranscodingTest, Reader1, Writer1, Reader2, Writer2,
        unittest::NestedWithBase, unittest::NestedWithBase, Protocols>(suite, "Transcoding with inheritance");

    AddTestCase<TEST_ID(N),
        DefaultValuesTranscoding, Reader1, Writer1, Reader2, Writer2, Protocols>(suite, "Transcoding default values");

    AddTestCase<TEST_ID(N),
        OmittedDefaultValuesTranscoding, Reader1, Writer1, Reader2, Writer2, Protocols>(suite, "Transcoding omitted values");
}


void PassThroughTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        PassThroughTests<
            0x701,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Pass-through tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        PassThroughTests<
            0x702,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Pass-through tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        PassThroughTests<
            0x703,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Pass-through tests for FastBinary");
    );

    TEST_SIMPLE_PROTOCOL(
    TEST_COMPACT_BINARY_PROTOCOL(
        PassThroughTests<
            0x704,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer>,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("CompactBinary-SimpleBinary pass-through");
    ));


    TEST_FAST_BINARY_PROTOCOL(
    TEST_COMPACT_BINARY_PROTOCOL(
        PassThroughTests<
            0x707,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer>,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("CompactBinary-FastBinary pass-through");
    ));


    TEST_COMPACT_BINARY_PROTOCOL(
        TranscodingTests<
            0x706,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer>,
            UntaggedProtocolReader<bond::InputBuffer>,
            UntaggedProtocolWriter<bond::OutputBuffer>,
            bond::BuiltInProtocols::Append<UntaggedProtocolReader<bond::InputBuffer> > >("CompactBinary-UntaggedProtocol transcoding");
    );


    TEST_COMPACT_BINARY_PROTOCOL(
    TEST_SIMPLE_JSON_PROTOCOL(
        TranscodingTests<
            0x710,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer>,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("CompactBinary-Simple JSON transcoding");
    ));


    TranscodingTests<
        0x709,
        UntaggedProtocolReader<bond::InputBuffer>,
        UntaggedProtocolWriter<bond::OutputBuffer>,
        UntaggedProtocolReader<bond::InputBuffer>,
        UntaggedProtocolWriter<bond::OutputBuffer>,
        bond::BuiltInProtocols::Append<UntaggedProtocolReader<bond::InputBuffer> > >("UntaggedProtocol-UntaggedProtocol transcoding");
}

bool init_unit_test()
{
    PassThroughTestsInit();
    return true;
}
