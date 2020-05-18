#ifdef _MSC_VER
#pragma warning(push)
// Since we're explicitly testing some exceptional code paths, some
// expansions of StaticParser are expected to produce unreachable code.
#pragma warning(disable: 4702) // C4702: unreachable code
#endif

#include "precompiled.h"
#include "serialization_test.h"
#include "untagged_protocol.h"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#ifdef _MSC_VER
// Some of the type names for the boost::mpl::list of schema fields in these
// tests are too long for MSVC to handle without truncation.
#pragma warning(disable: 4503) // warning C4503: '...' decorated name length exceeded, name was truncated
#endif

template <typename Protocols>
bool Compare(const Required& left, const RequiredViewGood& right)
{
    return Compare<Protocols>(left.x, right.x)
        && Equal<Protocols>(left.x2, right.x2)
        && Compare<Protocols>((uint64_t)left.z2, right.z2);
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols>
void RequiredFieldsTest(const From& from)
{
    typename Writer::Buffer buffer(1024);
    Writer writer(buffer);

    bond::Serialize<Protocols>(from, writer);

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<From, Reader&> bonded(reader);

        bonded.template Deserialize<Protocols>(to);
        UT_Equal(from, to);
    }

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<void, Reader&> bonded(reader, bond::GetRuntimeSchema<From>());

        bonded.template Deserialize<Protocols>(to);
        UT_Equal(from, to);
    }
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols>
TEST_CASE_BEGIN(RequiredFields)
{
    RequiredFieldsTest<Reader, Writer, From, To, Protocols>(From());
    RequiredFieldsTest<Reader, Writer, From, To, Protocols>(InitRandom<From, Protocols>());
}
TEST_CASE_END


template <typename Reader, typename Writer, typename From, typename To, typename Protocols>
void MissingRequiredFieldsTest(const From& from)
{
    typename Writer::Buffer buffer(1024);
    Writer writer(buffer);

    bond::Serialize<Protocols>(from, writer);

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<From, Reader&> bonded(reader);

        UT_AssertThrows((bonded.template Deserialize<Protocols>(to)), bond::CoreException);
    }

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<void, Reader&> bonded(reader, bond::GetRuntimeSchema<From>());

        UT_AssertThrows((bonded.template Deserialize<Protocols>(to)), bond::CoreException);
    }
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols>
TEST_CASE_BEGIN(MissingRequiredFields)
{
    MissingRequiredFieldsTest<Reader, Writer, From, To, Protocols>(From());
    MissingRequiredFieldsTest<Reader, Writer, From, To, Protocols>(InitRandom<From, Protocols>());
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T, typename Protocols>
TEST_CASE_BEGIN(OptionalToRequired)
{
    typedef BondStructOptional<T> From;
    typedef BondStructRequired<T> To;

    From from;

    typename Writer::Buffer buffer(1024);

    Factory<Writer>::Call(buffer, bond::v1, boost::bind(
        bond::Serialize<Protocols, From, Writer>, from, boost::placeholders::_1));

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<To, Reader&> bonded(reader);

        UT_AssertThrows((bonded.template Deserialize<Protocols>(to)), bond::CoreException);
    }

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<void, Reader&> bonded(reader, bond::GetRuntimeSchema<To>());

        UT_AssertThrows((bonded.template Deserialize<Protocols>(to)), bond::CoreException);
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer, typename Protocols = bond::BuiltInProtocols>
void RequiredTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        RequiredFields, Reader, Writer, Required, Required, Protocols>(suite, "Struct with required fields");

    AddTestCase<TEST_ID(N),
        RequiredFields, Reader, Writer, Required, RequiredViewGood, Protocols>(suite, "View with required fields");

    AddTestCase<TEST_ID(N),
        RequiredFields, Reader, Writer, RequiredViewGood, RequiredViewGood, Protocols>(suite, "Required and optional fields");

    AddTestCase<TEST_ID(N),
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMissingLast, Protocols>(suite, "Missing last required field");

    AddTestCase<TEST_ID(N),
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMissingFirst, Protocols>(suite, "Missing first required field");

    AddTestCase<TEST_ID(N),
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMismatchType, Protocols>(suite, "Mimatched type required field");

    AddTestCase<TEST_ID(N),
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMissingInNested, Protocols>(suite, "Required in nested fields");

    AddTestCase<TEST_ID(N),
        MissingRequiredFields, Reader, Writer, bond::Void, RequiredViewMissingLast, Protocols>(suite, "Missing all fields");

    if (bond::may_omit_fields<Writer>::value)
    {
        AddTestCase<TEST_ID(N),
            OptionalToRequired, Reader, Writer, bool, Protocols>(suite, "Optional bool to required");

        AddTestCase<TEST_ID(N),
            OptionalToRequired, Reader, Writer, list<float>, Protocols>(suite, "Optional list to required");
    }
}


void RequiredTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        RequiredTests<
            0x1401,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Required fields tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        RequiredTests<
            0x1402,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Required fields tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        RequiredTests<
            0x1403,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Required fields tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        RequiredTests<
            0x1404,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Required fields tests for Simple JSON");
    );

        RequiredTests<
            0x1405,
            UntaggedProtocolReader<bond::InputBuffer>,
            UntaggedProtocolWriter<bond::OutputBuffer>,
            bond::BuiltInProtocols::Append<UntaggedProtocolReader<bond::InputBuffer> > >("Required fields tests for untagged protocol");
}

bool init_unit_test()
{
    RequiredTestsInit();
    return true;
}
