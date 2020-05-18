#include "precompiled.h"
#include "serialization_test.h"


template <typename Writer, typename T>
void MarshalingTest(uint16_t version = bond::v1)
{
    T from = InitRandom<T>();
    typename Writer::Buffer output_buffer;
    
    Factory<Writer>::Call(output_buffer, version, boost::bind(
        bond::Marshal<bond::BuiltInProtocols, T, Writer>, from, boost::placeholders::_1));
    
    T to;
    bond::InputBuffer input(output_buffer.GetBuffer());
    
    bond::Unmarshal(input, to);
    UT_Compare(from, to);

    auto to2 = bond::Unmarshal<T>(input);
    UT_Compare(from, to2);
}


template <typename Writer, typename T>
TEST_CASE_BEGIN(Marshaling)
{
    MarshalingTest<Writer, T>();
    MarshalingTest<Writer, T>(Writer::Reader::version);
}
TEST_CASE_END


template <typename Writer, typename T>
void TranscodingTest(uint16_t version = bond::v1)
{
    T from = InitRandom<T>();
    typename Writer::Buffer output_buffer;

    Factory<Writer>::Call(output_buffer, version, boost::bind(
        bond::Marshal<bond::BuiltInProtocols, T, Writer>, from, boost::placeholders::_1));
    
    // Trans-marshal to Simple Protocol using runtime schema
    bond::OutputBuffer simple_buffer;
    bond::SimpleBinaryWriter<bond::OutputBuffer> simple_writer(simple_buffer, Writer::Reader::version);

    {
        bond::InputBuffer input(output_buffer.GetBuffer());
        bond::ProtocolType proto = bond::SelectProtocolAndApply(bond::GetRuntimeSchema<T>(), input, bond::MarshalTo(simple_writer)).first;

        UT_AssertAreEqual(proto, Writer::Reader::magic);
    }

    {
        // Trans-marshal back to Writer using compile-time schema
        typename Writer::Buffer writer_buffer;

        {
            bond::InputBuffer input(simple_buffer.GetBuffer());
	    Factory<Writer>::Call(writer_buffer, version, boost::bind(
                 bond::SelectProtocolAndApply<T, bond::BuiltInProtocols, bond::InputBuffer, bond::Marshaler<Writer> >, input, boost::bind(bond::MarshalTo<bond::BuiltInProtocols, Writer>, boost::placeholders::_1)));
        }

        T to;

        bond::InputBuffer input(writer_buffer.GetBuffer());
        Unmarshal(input, to);

        UT_Compare(from, to);
    }

    {
        // Trans-marshal back to Writer using runtime schema
        typename Writer::Buffer writer_buffer;

        {
            bond::InputBuffer input(simple_buffer.GetBuffer());
	    Factory<Writer>::Call(writer_buffer, version, boost::bind(
                 bond::SelectProtocolAndApply<bond::BuiltInProtocols, bond::InputBuffer, bond::Marshaler<Writer> >, bond::GetRuntimeSchema<T>(), input, boost::bind(bond::MarshalTo<bond::BuiltInProtocols, Writer>, boost::placeholders::_1)));
        }

        T to;

        bond::InputBuffer input(writer_buffer.GetBuffer());
        Unmarshal(input, to);

        UT_Compare(from, to);
    }
}


template <typename Writer, typename T>
TEST_CASE_BEGIN(Transcoding)
{
    TranscodingTest<Writer, T>();
    TranscodingTest<Writer, T>(Writer::Reader::version);
}
TEST_CASE_END


template <typename Writer, typename From, typename To>
TEST_CASE_BEGIN(MapTo)
{
    From from = InitRandom<From>();
    typename Writer::Buffer output_buffer;
    Writer output(output_buffer);

    bond::Marshal(from, output);
    
    bond::Mappings mappings;

    InitMappings<To, From>(mappings);
    
    // Mapping with compile-time schema
    {
        To to;

        bond::InputBuffer input(output_buffer.GetBuffer());
        bond::SelectProtocolAndApply<From, bond::BuiltInProtocols>(input, bond::MapTo<To>(to, mappings));

        UT_Compare(from, to);
    }
    
    // Mapping with runtime time schema
    {
        To to;

        bond::InputBuffer input(output_buffer.GetBuffer());
        bond::SelectProtocolAndApply(bond::GetRuntimeSchema<From>(), input, bond::MapTo<To>(to, mappings));

        UT_Compare(from, to);
    }
}
TEST_CASE_END


template <uint16_t N, typename Writer>
void MarshalTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        Marshaling, Writer, NestedWithBase>(suite, "Marshaling struct");

    TEST_SIMPLE_PROTOCOL(
        AddTestCase<TEST_ID(N), 
            Transcoding, Writer, NestedWithBase>(suite, "Transcoding marshaled data");
    );

    AddTestCase<TEST_ID(N), 
        MapTo, Writer, NestedWithBase, NestedWithBase>(suite, "Map marshaled data");

    AddTestCase<TEST_ID(N), 
        MapTo, Writer, NestedWithBase, NestedWithBase2>(suite, "Map marshaled data to base");

    AddTestCase<TEST_ID(N), 
        MapTo, Writer, NestedWithBase, NestedWithBaseView>(suite, "Map marshaled data to view");
}


void MarshalTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        MarshalTests<
            0x1801,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Marshal tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        MarshalTests<
            0x1802,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Marshal tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        MarshalTests<
            0x1803,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Marshal tests for FastBinary");
    );
}

bool init_unit_test()
{
    MarshalTestsInit();
    return true;
}

