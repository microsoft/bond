#include "precompiled.h"
#include "serialization_test.h"
#include "untagged_protocol.h"
#include <boost/mpl/size.hpp>


template <class Reader> struct
rebind_buffer_by_reference;

template <template <typename T> class Reader, typename Buffer> struct
rebind_buffer_by_reference<Reader<Buffer> >
{
    typedef Reader<Buffer&> type;
};


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(Streaming)
{
    T from1, from2;

    InitRandom(from1);
    InitRandom(from2);

    bond::blob data;

    {
        typename Writer::Buffer buffer;
        Writer writer(buffer);

        // Serialize 2 records
        bond::Serialize(from1, writer);
        bond::Serialize(from2, writer);

        data = buffer.GetBuffer();
    }

    {
        Reader reader(data);
        bond::bonded<T, Reader&> bonded(reader);

        T to1, to2;

        bonded.Deserialize(to1);

        UT_AssertIsFalse(reader.GetBuffer().IsEof());

        bonded.Deserialize(to2);

        UT_AssertIsTrue(reader.GetBuffer().IsEof());

        UT_AssertIsTrue(from1 == to1);
        UT_AssertIsTrue(from2 == to2);
    }

    {
        typename Reader::Buffer buffer(data);
        typename rebind_buffer_by_reference<Reader>::type reader(buffer);

        T to1, to2;

        Deserialize(reader, to1);

        UT_AssertIsFalse(buffer.IsEof());

        Deserialize(reader, to2);

        UT_AssertIsTrue(buffer.IsEof());

        UT_AssertIsTrue(from1 == to1);
        UT_AssertIsTrue(from2 == to2);
    }
}
TEST_CASE_END


// 1 bit per every 8 fields
// This simplified count obviously doesn't work for nested structures but
// the tests that check payload length don't use nested structs.
template <typename T> struct
untagged_payload_size
{
    static const unsigned value = (boost::mpl::size<typename T::Schema::fields>::value + 7) / 8
                                + untagged_payload_size<typename T::Schema::base>::value;
};

template <typename T>
const unsigned untagged_payload_size<T>::value;

template <> struct
untagged_payload_size<bond::no_base>
{
    static const unsigned value = 0;
};


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(DefaultValues)
{
    T obj;

    // OutputBuffer is not really designed to allow writing before current pointer
    // which is required for our test untagged protocol. However it works if
    // memory for OutputBuffer is preallocated.
    typename Writer::Buffer output_buffer(untagged_payload_size<T>::value);
    Writer output(output_buffer);

    // serialize value to output
    bond::Serialize(obj, output);

    if (bond::uses_dynamic_parser<Reader>::value)
    {
        UT_AssertAreEqual((bond::has_base<T>::value ? 2u : 1u), output_buffer.GetBuffer().length());
    }
    else
    {
        if (std::is_same<Reader, UntaggedProtocolReader<typename Reader::Buffer> >::value)
            UT_AssertAreEqual(untagged_payload_size<T>::value, output_buffer.GetBuffer().length());
        else
            UT_AssertIsTrue(output_buffer.GetBuffer().length() != 1);
    }

    T to;
    Reader reader(output_buffer.GetBuffer());

    bond::Deserialize(reader, to);
    UT_AssertIsTrue(obj == to);
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(SerializeAPIs)
{
    T from = InitRandom<T>();

    typename Writer::Buffer output;
    Writer writer(output);

    Serialize(from, writer);

    Reader reader(output.GetBuffer());

    {
        T to;
        Deserialize(reader, to);
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        T to;
        Deserialize(reader, to, bond::GetRuntimeSchema<T>());
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        auto to = bond::Deserialize<T>(reader);
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        auto to = bond::Deserialize<T>(reader, bond::GetRuntimeSchema<T>());
        UT_AssertIsTrue(Equal(from, to));
    }

}
TEST_CASE_END


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(MarshalAPIs)
{
    T from = InitRandom<T>();

    typename Writer::Buffer output;
    Writer writer(output);

    Marshal(from, writer);

    typename Reader::Buffer input(output.GetBuffer());

    {
        T to;
        Unmarshal(input, to);
        UT_AssertIsTrue(from == to);
    }

    {
        bond::bonded<T> to;
        Unmarshal(input, to);
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        T to;
        Unmarshal(input, to, bond::GetRuntimeSchema<T>());
        UT_AssertIsTrue(from == to);
    }

    {
        bond::bonded<T> to;
        Unmarshal(input, to, bond::GetRuntimeSchema<T>());
        UT_AssertIsTrue(Equal(from, to));
    }

    {
        auto to = bond::Unmarshal<T>(input);
        UT_AssertIsTrue(from == to);
    }

    {
        auto to = bond::Unmarshal<T>(input, bond::GetRuntimeSchema<T>());
        UT_AssertIsTrue(from == to);
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void SimpleStructTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        SerializeAPIs, Reader, Writer, NestedStruct>(suite, "De/serialization APIs");

    AddTestCase<COND_TEST_ID(N, (!bond::uses_dom_parser<Reader>::value)),
        MarshalAPIs, Reader, Writer, NestedStruct>(suite, "Un/marshal APIs");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, SimpleStruct>(suite, "Simple struct");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, UsingImport>(suite, "Imported struct");

    AddTestCase<TEST_ID(N),
        DefaultValues, Reader, Writer, StructWithDefaults>(suite, "Omitting default values");

    AddTestCase<TEST_ID(N),
        DefaultValues, Reader, Writer, OptionalContainers>(suite, "Omitting empty containers");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, NestedStruct1, NestedStruct1OptionalBondedView>(suite, "Optional bonded field");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, SimpleStruct, SimpleStructView>(suite, "Simple struct partial view");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, NestedStruct>(suite, "Nested struct");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, NestedStruct, NestedStructView>(suite, "Nested struct partial view");

    AddTestCase<COND_TEST_ID(N, (!bond::uses_dom_parser<Reader>::value)),
        Streaming, Reader, Writer, SimpleStruct>(suite, "Record streaming");

    AddTestCase<TEST_ID(N),
        SerializeAPIs, Reader, Writer, EnumValueWrapper>(suite, "Struct with alias-wrapped enum");
}


template <uint16_t N, typename Reader, typename Writer>
void OmittingDefaultsTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        DefaultValues, Reader, Writer, StructWithDefaults>(suite, "Omitting default values");

    AddTestCase<TEST_ID(N),
        DefaultValues, Reader, Writer, OptionalContainers>(suite, "Omitting empty containers");

    AddTestCase<TEST_ID(N),
        DefaultValues, Reader, Writer, OptionalNothing>(suite, "Omitting nothing");
}


void SerializationTest::SimpleStructTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        SimpleStructTests<
            0x901,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Struct deserialization tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        SimpleStructTests<
            0x902,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Struct deserialization tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        SimpleStructTests<
            0x903,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Struct deserialization tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        SimpleStructTests<
            0x904,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Struct deserialization tests for Simple JSON");
    );

        OmittingDefaultsTests<
            0x905,
            UntaggedProtocolReader<bond::InputBuffer>,
            UntaggedProtocolWriter<bond::OutputBuffer> >("Omitting defaults for untagged protocol");
}


bool init_unit_test()
{
    SerializationTest::Initialize();
    return true;
}
