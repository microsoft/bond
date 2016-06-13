#pragma once

#include <bond/core/customize.h>
#include <bond/protocol/compact_binary.h>
#include "untagged_protocol.h"
#include <boost/mpl/list.hpp>
#include <boost/mpl/push_front.hpp>

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

        TestReader(typename boost::call_traits<Buffer>::param_type input)
            : bond::CompactBinaryReader<Buffer>(input)
        {}

        bool operator==(const TestReader& rhs) const
        {
            return bond::CompactBinaryReader<Buffer>::operator==(rhs);
        }

        using bond::CompactBinaryReader<Buffer>::Skip;

        template <typename T>
        void Skip(const bond::bonded<T, TestReader&>&)
        {
            Skip(bond::BondDataType::BT_STRUCT);
        }
    };

    template <typename Buffer>
    class TestWriter
        : public bond::CompactBinaryWriter<Buffer>
    {
    public:
        typedef bond::DynamicParser<TestWriter&> Parser;

        TestWriter(Buffer& output)
            : bond::CompactBinaryWriter<Buffer>(output)
        {}

        void WriteStructBegin(const bond::Metadata& /*metadata*/, bool /*base*/)
        {}
    };
}

namespace bond
{
    // Add TestReader to the list of protocols used by Bond
    template <> struct
    customize<protocols>
    {
        template <typename T> struct
        modify
        {
            typedef typename boost::mpl::push_front<
                T, unit_test::TestReader<InputBuffer>
            >::type type1;
 
            typedef typename boost::mpl::push_front<
                type1, UntaggedProtocolReader<InputBuffer>
            >::type type;
        };
    };

    // Protocols are disabled by default and we leave TestReader disabled
    // and enable it only in custom_protocol.cpp and UntaggedProtocolReader
    // in pass_through.cpp.
}

namespace bond
{
    // Enable UntaggedProtocolReader in this file
    template <typename Buffer> struct 
    is_protocol_enabled<UntaggedProtocolReader<Buffer> >
    {
        static const bool value = true;
    };
}
