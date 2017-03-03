#include "precompiled.h"
#include "json_tests.h"
#include <locale>
#include <stdarg.h>

using namespace bond;


template <typename From, typename To>
const To& operator->*(const From& from, To& to)
{
    bond::OutputBuffer buffer;
    bond::SimpleJsonWriter<bond::OutputBuffer> json_writer(buffer);

    bond::Serialize(from, json_writer);

    bond::SimpleJsonReader<bond::InputBuffer> json_reader(buffer.GetBuffer());

    bond::Deserialize(json_reader, to);

    return to;
}


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(StringRoundtripTest)
{
    BondStruct<std::string> str1, str2;
    BondStruct<std::wstring> wstr1, wstr2;

    str1.field += "Arabic: \xD9\x85\xD8\xB1\xD8\xAD\xD8\xA8\xD8\xA7 \xD8\xA7\xD9\x84\xD8\xB9\xD8\xA7\xD9\x84\xD9\x85 | ";
    str1.field += "Chinese: \xE4\xBD\xA0\xE5\xA5\xBD\xE4\xB8\x96\xE7\x95\x8C | ";
    str1.field += "Hebrew: \xD7\xA9\xD7\x9C\xD7\x95\xD7\x9D \xD7\xA2\xD7\x95\xD7\x9C\xD7\x9D | ";
    str1.field += "Japanese: \xE3\x81\x93\xE3\x82\x93\xE3\x81\xAB\xE3\x81\xA1\xE3\x81\xAF\xE4\xB8\x96\xE7\x95\x8C | ";
    str1.field += "Russian: \xD0\x9F\xD1\x80\xD0\xB8\xD0\xB2\xD0\xB5\xD1\x82 \xD0\xBC\xD0\xB8\xD1\x80 | ";
    str1.field += "Escaped: \" \\ / \b \f \n \r \t \x1 \x1f ";
    str1.field += '\x0';

    str1 ->* wstr1 ->* str2 ->* wstr2;

    UT_AssertIsTrue(str1 == str2);
    UT_AssertIsTrue(wstr1 == wstr2);
}
TEST_CASE_END


template <typename T, typename Reader, typename Writer>
TEST_CASE_BEGIN(StreamDeserializationTest)
{
    const int count = 10;
    T from[count];

    typename Writer::Buffer output;

    // Serialize random objects
    {
        Writer writer(output);

        for (int i = count; i--;)
        {
            from[i] = InitRandom<T>();
            Serialize(from[i], writer);
        }
    }

    // Deserialize the objects
    {
        Reader reader(output.GetBuffer());
        bond::bonded<T, Reader&> stream(reader);

        for (int i = count; i--;)
        {
            T record = InitRandom<T>();

            stream.Deserialize(record);
            UT_AssertIsTrue(Equal(from[i], record));
        }
    }

    // Deserialize the first object twice
    {
        Reader reader(output.GetBuffer());

        T r1, r2;
        r1 = InitRandom<T>();
        r2 = InitRandom<T>();
        Deserialize(reader, r1);
        Deserialize(reader, r2);
        UT_AssertIsTrue(Equal(r1, r2));

        bond::bonded<T> bonded(reader);
        r1 = InitRandom<T>();
        r2 = InitRandom<T>();
        bonded.Deserialize(r1);
        bonded.Deserialize(r2);
        UT_AssertIsTrue(Equal(r1, r2));
    }
}
TEST_CASE_END


template <typename Record, typename Intermediate, typename Reader1, typename Writer1, typename Reader2, typename Writer2>
void StreamTranscoding(uint16_t version = bond::v1)
{
    const int count = 10;

    Record records[count];

    // Serialize random objects using protocol 1
    typename Writer1::Buffer output1;
    Writer1 writer1(output1);

    for (int i = count; i--;)
    {
        records[i] = InitRandom<Record>();
        Serialize(records[i], writer1);
    }

    // Tanscode the objects from protocol 1 to protocol 2
    Reader1 reader1(output1.GetBuffer());

    typename Writer2::Buffer output2;
    Writer2 writer2(output2, version);

    for (int i = count; i--;)
    {
        bond::bonded<Intermediate> record;

        bond::bonded<Intermediate, Reader1&>(reader1).Deserialize(record);
        Serialize(record, writer2);
    }

    // Deserialize the objects from protocol 2
    Reader2 reader2(output2.GetBuffer(), version);
    bond::bonded<Record, Reader2&> stream(reader2);

    for (int i = count; i--;)
    {
        Record record = InitRandom<Record>();

        stream.Deserialize(record);
        UT_AssertIsTrue(Equal(records[i], record));
    }
}


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(StreamTranscodingTest)
{
    StreamTranscoding<
        StructWithBase,
        SimpleStruct,
        Reader,
        Writer,
        bond::CompactBinaryReader<bond::InputBuffer>,
        bond::CompactBinaryWriter<bond::OutputBuffer>
    >(bond::v1);

    StreamTranscoding<
        StructWithBase,
        SimpleStruct,
        Reader,
        Writer,
        bond::CompactBinaryReader<bond::InputBuffer>,
        bond::CompactBinaryWriter<bond::OutputBuffer>
    >(bond::v2);

    StreamTranscoding<
        NestedStruct,
        NestedStructBondedView,
        Reader,
        Writer,
        bond::CompactBinaryReader<bond::InputBuffer>,
        bond::CompactBinaryWriter<bond::OutputBuffer>
    >(bond::v1);

    StreamTranscoding<
        NestedStruct,
        NestedStructBondedView,
        Reader,
        Writer,
        bond::CompactBinaryReader<bond::InputBuffer>,
        bond::CompactBinaryWriter<bond::OutputBuffer>
    >(bond::v2);
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void StringTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        StringRoundtripTest, Reader, Writer>(suite, "Roundtrip string/wstring");

    AddTestCase<TEST_ID(N),
        StreamDeserializationTest, NestedStruct, Reader, Writer>(suite, "Stream deserialization test");
}


void JSONTest::Initialize()
{
    TEST_SIMPLE_JSON_PROTOCOL(
        StringTests<
            0x1c04,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Simple JSON test");
    );
}

bool init_unit_test()
{
    JSONTest::Initialize();
    return true;
}
