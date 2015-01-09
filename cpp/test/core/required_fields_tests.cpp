#include "precompiled.h"
#include "serialization_test.h"
#include "untagged_protocol.h"

namespace unittest
{

inline bool operator==(const Required& left, const RequiredViewGood& right)
{
    return left.x == right.x
        && Equal(left.x2, right.x2)
        && (uint64_t)left.z2 == right.z2;
}

}

template <typename Reader, typename Writer, typename From, typename To>
void RequiredFieldsTest(const From& from)
{
    typename Writer::Buffer buffer(1024);
    Writer writer(buffer);

    bond::Serialize(from, writer);

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<From, Reader&> bonded(reader);

        bonded.Deserialize(to);
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<void, Reader&> bonded(reader, bond::GetRuntimeSchema<From>());

        bonded.Deserialize(to);
        UT_AssertIsTrue(Equal(from, to));
    }
}


template <typename Reader, typename Writer, typename From, typename To>
TEST_CASE_BEGIN(RequiredFields)
{
    RequiredFieldsTest<Reader, Writer, From, To>(From());
    RequiredFieldsTest<Reader, Writer, From, To>(InitRandom<From>());
}
TEST_CASE_END


template <typename Reader, typename Writer, typename From, typename To>
void MissingRequiredFieldsTest(const From& from)
{
    typename Writer::Buffer buffer(1024);
    Writer writer(buffer);

    bond::Serialize(from, writer);

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<From, Reader&> bonded(reader);

        UT_AssertThrows((bonded.Deserialize(to)), bond::CoreException);
    }

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<void, Reader&> bonded(reader, bond::GetRuntimeSchema<From>());

        UT_AssertThrows((bonded.Deserialize(to)), bond::CoreException);
    }
}


template <typename Reader, typename Writer, typename From, typename To>
TEST_CASE_BEGIN(MissingRequiredFields)
{
    MissingRequiredFieldsTest<Reader, Writer, From, To>(From());
    MissingRequiredFieldsTest<Reader, Writer, From, To>(InitRandom<From>());
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(OptionalToRequired)
{
    typedef BondStructOptional<T> From;
    typedef BondStructRequired<T> To;
    
    From from;
    
    typename Writer::Buffer buffer(1024);
    
    Factory<Writer>::Call(buffer, bond::v1, boost::bind(
        bond::Serialize<From, Writer>, from, _1));

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<To, Reader&> bonded(reader);

        UT_AssertThrows((bonded.Deserialize(to)), bond::CoreException);
    }

    {
        To to;
        Reader reader(buffer.GetBuffer());
        bond::bonded<void, Reader&> bonded(reader, bond::GetRuntimeSchema<To>());

        UT_AssertThrows((bonded.Deserialize(to)), bond::CoreException);
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void RequiredTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        RequiredFields, Reader, Writer, Required, Required>(suite, "Struct with required fields");
    
    AddTestCase<TEST_ID(N), 
        RequiredFields, Reader, Writer, Required, RequiredViewGood>(suite, "View with required fields");

    AddTestCase<TEST_ID(N), 
        RequiredFields, Reader, Writer, RequiredViewGood, RequiredViewGood>(suite, "Required and optional fields");

    AddTestCase<TEST_ID(N), 
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMissingLast>(suite, "Missing last required field");

    AddTestCase<TEST_ID(N), 
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMissingFirst>(suite, "Missing first required field");

    AddTestCase<TEST_ID(N), 
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMismatchType>(suite, "Mimatched type required field");

    AddTestCase<TEST_ID(N), 
        MissingRequiredFields, Reader, Writer, Required, RequiredViewMissingInNested>(suite, "Required in nested fields");
    
    AddTestCase<TEST_ID(N), 
        MissingRequiredFields, Reader, Writer, bond::Void, RequiredViewMissingLast>(suite, "Missing all fields");

    if (bond::may_omit_fields<Writer>::value)
    {
        AddTestCase<TEST_ID(N), 
            OptionalToRequired, Reader, Writer, bool>(suite, "Optional bool to required");

        AddTestCase<TEST_ID(N), 
            OptionalToRequired, Reader, Writer, list<float> >(suite, "Optional list to required");
    }
}


void SerializationTest::RequiredTestsInit()
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
            UntaggedProtocolWriter<bond::OutputBuffer> >("Required fields tests for untagged protocol");
}

