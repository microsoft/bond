#include "precompiled.h"
#include "protocol_test.h"


// Test encoding/decoding for specified value of particular type
template <typename Reader, typename Writer, typename T>
void TestEncoding(const std::vector<T>& data)
{
    typename Writer::Buffer output_buffer;
    Writer output(output_buffer);

    for(size_t i = 0; i < data.size(); ++i)
    {
        output.Write(data[i]);
    }

    typename Reader::Buffer input_buffer(output_buffer.GetBuffer());
    Reader input(input_buffer);

    for(size_t i = 0; i < data.size(); ++i)
    {
        T value;

        input.Read(value);

        UT_AssertIsTrue(data[i] == value);
    }
}


// Tests encoding/decoding for ALL values for an integer type
template <typename T, typename Reader, typename Writer>
void TestEncodingInteger()
{
    typename Writer::Buffer output_buffer;
    Writer output(output_buffer);

    for(uint64_t i = 0; i <= typename boost::make_unsigned<T>::type(-1); ++i)
    {
        output.Write(T(i));
    }

    typename Reader::Buffer input_buffer(output_buffer.GetBuffer());
    Reader input(input_buffer);

    for(uint64_t i = 0; i <= typename boost::make_unsigned<T>::type(-1); ++i)
    {
        T value;

        input.Read(value);

        UT_AssertAreEqual(T(i), value);
    }
}


// Test encoding for ALL signed/unsigned 8 and 16 bit integers
// and "interesting" values for 32 and 64 bit integers
template <typename Reader, typename Writer>
TEST_CASE_BEGIN(IntegerEncoding)
{
    TestEncoding<Reader, Writer>(IntegerConstants<uint64_t>());
    TestEncoding<Reader, Writer>(IntegerConstants<int64_t>());
    TestEncoding<Reader, Writer>(IntegerConstants<uint32_t>());
    TestEncoding<Reader, Writer>(IntegerConstants<int32_t>());
    
    TestEncodingInteger<uint8_t, Reader, Writer>();
    TestEncodingInteger<int8_t, Reader, Writer>();

    TestEncodingInteger<uint16_t, Reader, Writer>();
    TestEncodingInteger<int16_t, Reader, Writer>();
}
TEST_CASE_END


// Test encoding/decoding for all field ids with a specified type
template <typename Reader, typename Writer, typename Enable = void>
struct FieldBeginEncoder
{
    template <typename T>
    void operator()(const T&)
    {
        typename Writer::Buffer output_buffer;
        Writer output(output_buffer);

        for(uint16_t i = 0; i < USHRT_MAX/2; ++i)
        {
            output.WriteFieldBegin(bond::get_type_id<T>::value, i);
        }

        for(uint16_t i = USHRT_MAX; i >= USHRT_MAX/2; i -= 3)
        {
            output.WriteFieldBegin(bond::get_type_id<T>::value, i);
        }

        typename Reader::Buffer input_buffer(output_buffer.GetBuffer());
        Reader input(input_buffer);
        uint16_t id;
        bond::BondDataType type;

        for(uint16_t i = 0; i < USHRT_MAX/2; ++i)
        {
            input.ReadFieldBegin(type, id);

            UT_AssertAreEqual(bond::get_type_id<T>::value, type);
            UT_AssertAreEqual(i, id);
        }

        for(uint16_t i = USHRT_MAX; i >= USHRT_MAX/2; i -= 3)
        {
            input.ReadFieldBegin(type, id);

            UT_AssertAreEqual(bond::get_type_id<T>::value, type);
            UT_AssertAreEqual(i, id);
        }
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(FieldBeginEncoding)
{
    boost::mpl::for_each<NumericTypes>(FieldBeginEncoder<Reader, Writer>());
}
TEST_CASE_END


template <typename Buffer>
void WritePass0(Buffer& pass0, uint16_t val0, int64_t val1)
{
    pass0.WriteStructBegin(bond::Metadata(), false);
    pass0.WriteFieldBegin(bond::BT_UINT16, 0);
    pass0.Write(val0);
    pass0.WriteFieldEnd();

    pass0.WriteFieldBegin(bond::BT_INT64, 1);
    pass0.Write(val1);
    pass0.WriteFieldEnd();
    pass0.WriteStructEnd();
}


template <typename Reader, typename Writer, typename Buffer>
void TestReadStruct(Writer& output, Buffer& output_buffer, uint16_t val0, int64_t val1)
{
    output.WriteStructBegin(bond::Metadata(), false);
    output.WriteFieldBegin(bond::BT_UINT16, 0);
    output.Write(val0);
    output.WriteFieldEnd();

    output.WriteFieldBegin(bond::BT_INT64, 1);
    output.Write(val1);
    output.WriteFieldEnd();
    output.WriteStructEnd();

    // read from output, using CB version 2
    typename Reader::Buffer input_buffer(output_buffer.GetBuffer());
    Reader input(input_buffer, 2);
    uint16_t id;
    bond::BondDataType type;
    uint16_t value0;
    int64_t value1;

    input.ReadStructBegin();
    input.ReadFieldBegin(type, id);
    input.Read(value0);
    input.ReadFieldEnd();
    UT_AssertAreEqual(type, bond::BT_UINT16);
    UT_AssertAreEqual(id, 0);
    UT_AssertAreEqual(val0, value0);

    input.ReadFieldBegin(type, id);
    input.Read(value1);
    input.ReadFieldEnd();
    input.ReadStructEnd();
    UT_AssertAreEqual(type, bond::BT_INT64);
    UT_AssertAreEqual(id, 1);
    UT_AssertAreEqual(val1, value1);

    // test skipping struct, using CB version 2
    typename Reader::Buffer input_buffer2(output_buffer.GetBuffer());
    Reader input2(input_buffer2, 2);   

    input2.Skip(bond::BT_STRUCT);
    UT_AssertIsTrue(input2.GetBuffer().IsEof());
}


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(StructLengthEncoding)
{
    // create writer using CB version 2
    typename Writer::Buffer output_buffer;
    Writer output(output_buffer, bond::v2);

    UT_AssertAreEqual(output.NeedPass0(), true);

    // pass 0, write to counter using CB version 2
    typename Writer::Pass0::Buffer output0;
    bond::CompactBinaryWriter<typename Writer::Pass0::Buffer> pass0(output0, bond::v2);
    uint16_t val0 = 1000;
    int64_t val1 = 987654321;
    WritePass0(pass0, val0, val1);

    // pass 1, write to output and read it back
    output.WithPass0(pass0), TestReadStruct<Reader, Writer>(output, output_buffer, val0, val1);
}
TEST_CASE_END

template <typename Reader, typename Writer>
TEST_CASE_BEGIN(StringEncoding)
{
    std::vector<string> data;

    // Empty string
    data.push_back(string(""));

    // String with embedded \x0
    string tmp = "String with embedded 0 >";
    tmp += '\x0';
    tmp += "<";
    data.push_back(tmp);

    // Some random strings
    bond::RandomProtocolReader rpr;
    for (int i = 0; i < 10; i++)
    {
        string s;
        rpr.Read(s);
        data.push_back(s);
    }

    TestEncoding<Reader, Writer>(data);
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void GenericProtocolTest(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        IntegerEncoding, Reader, Writer>(suite, "Integer encoding");
    
    AddTestCase<TEST_ID(N), 
        StringEncoding, Reader, Writer>(suite, "String encoding");
    
    AddTestCase<COND_TEST_ID(N, !bond::uses_static_parser<Reader>::value), 
        FieldBeginEncoding, Reader, Writer>(suite, "FieldBegin encoding");
    
    AddTestCase<COND_TEST_ID(N, (std::is_same<Writer, bond::CompactBinaryWriter<bond::OutputBuffer> >::value)), 
        StructLengthEncoding, Reader, Writer>(suite, "StructLength encoding");
}


void ProtocolTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        GenericProtocolTest<
            0x801,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("SimpleBinary protocol test");
    );
    
    TEST_COMPACT_BINARY_PROTOCOL(
        GenericProtocolTest<
            0x802,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("CompactBinary protocol test");
    );

    TEST_FAST_BINARY_PROTOCOL(
        GenericProtocolTest<
            0x803,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("FastBinary protocol test");
    );
}


bool init_unit_test()
{
    ProtocolTest::Initialize();
    return true;
}

