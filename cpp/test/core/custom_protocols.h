#pragma once

#include <bond/core/customize.h>
#include <bond/core/protocol.h>
#include <bond/protocol/compact_binary.h>
#include <bond/stream/input_buffer.h>

#include "untagged_protocol.h"


namespace unit_test
{
    template <typename Buffer>
    class TestWriter;

    // A test protocol reader which is just CB under different name
    template <typename Buffer>
    class TestReader
        : public bond::CompactBinaryReader<Buffer>
    {
    public:
        typedef bond::DynamicParser<TestReader&> Parser;
        typedef TestWriter<Buffer>               Writer;

        BOND_STATIC_CONSTEXPR uint16_t magic = 0xEEEE;

        TestReader(typename boost::call_traits<Buffer>::param_type input)
            : bond::CompactBinaryReader<Buffer>(input)
        {}

        bool ReadVersion()
        {
            uint16_t temp_magic;

            this->_input.Read(temp_magic);
            this->_input.Read(this->_version);

            return temp_magic == TestReader::magic
                && this->_version <= TestReader::version;
        }

        bool operator==(const TestReader& rhs) const
        {
            return bond::CompactBinaryReader<Buffer>::operator==(rhs);
        }

        using bond::CompactBinaryReader<Buffer>::Skip;

        template <typename T>
        void Skip(const bond::bonded<T, TestReader&>&)
        {
            Skip(bond::BT_STRUCT);
        }
    };

    template <typename Buffer>
    class TestWriter
        : public bond::CompactBinaryWriter<Buffer>
    {
    public:
        typedef bond::DynamicParser<TestWriter&> Parser;
        typedef TestReader<Buffer>               Reader;

        TestWriter(Buffer& output)
            : bond::CompactBinaryWriter<Buffer>(output)
        {}

        void WriteVersion()
        {
            this->_output.Write(Reader::magic);
            this->_output.Write(this->_version);
        }

        void WriteStructBegin(const bond::Metadata& /*metadata*/, bool /*base*/)
        {}
    };

    class CustomInputBuffer
    {
    public:
        CustomInputBuffer(const bond::blob& blob)
            : _buffer{ blob }
        {}

        bool operator==(const CustomInputBuffer& rhs) const
        {
            return _buffer == rhs._buffer;
        }

        template <typename T>
        void Read(T& value)
        {
            _buffer.Read(value);
        }

        void Read(void *buffer, uint32_t size)
        {
            _buffer.Read(buffer, size);
        }

        void Read(bond::blob& blob, uint32_t size)
        {
            _buffer.Read(blob, size);
        }

        void Skip(uint32_t size)
        {
            _buffer.Skip(size);
        }

        bool IsEof() const
        {
            return _buffer.IsEof();
        }

    private:
        friend bond::blob GetCurrentBuffer(const CustomInputBuffer& input)
        {
            return GetCurrentBuffer(input._buffer);
        }


        bond::InputBuffer _buffer;
    };
}

namespace bond
{
    BOND_DEFINE_BUFFER_MAGIC(unit_test::CustomInputBuffer, 0x4243 /*CB*/);

    template <typename Buffer> struct 
    is_protocol_enabled<UntaggedProtocolReader<Buffer> >
        : std::true_type {};

    template <typename Buffer> struct 
    is_protocol_enabled<unit_test::TestReader<Buffer> >
        : std::true_type {};
}
