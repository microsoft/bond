// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bond.h>
#include <bond/core/bonded.h>
#include <bond/core/reflection.h>
#include <bond/stream/output_buffer.h>

#include <grpcpp/support/byte_buffer.h>

#include <boost/assert.hpp>
#include <boost/make_shared.hpp>

#include <cstdint>
#include <cstdlib>
#include <limits>
#include <stdint.h>

namespace bond { namespace ext { namespace gRPC { namespace detail
{
    template <typename T>
    inline grpc::Status Serialize(const bonded<T>& msg, grpc::ByteBuffer& buffer, bool& own_buffer)
    {
        OutputBuffer output;
        CompactBinaryWriter<OutputBuffer> writer(output);

        msg.Serialize(writer);

        // TODO: Optimize and create grpc::Slice-s that keep the bond::blob-s alive
        // instead of merging and copying data.
        blob data = output.GetBuffer();
        grpc::Slice slice(data.data(), data.size());

        own_buffer = true;
        buffer = grpc::ByteBuffer(&slice, 1);

        return grpc::Status::OK;
    }

    template <typename T>
    inline grpc::Status Deserialize(grpc_byte_buffer* buffer, bonded<T>& msg)
    {
        const size_t bufferSize = grpc_byte_buffer_length(buffer);
        if (bufferSize > (std::numeric_limits<uint32_t>::max)())
        {
            grpc_byte_buffer_destroy(buffer);
            return { grpc::StatusCode::INTERNAL, "Buffer is too large" };
        }

        auto buff = boost::make_shared_noinit<char[]>(bufferSize);
        char* dest = buff.get();

        // TODO: exception safety of reader, buffer
        grpc_byte_buffer_reader reader;
        if (!grpc_byte_buffer_reader_init(&reader, buffer))
        {
            grpc_byte_buffer_destroy(buffer);
            return { grpc::StatusCode::INTERNAL, "Failed to init buffer reader" };
        }

        for (grpc_slice grpc_s; grpc_byte_buffer_reader_next(&reader, &grpc_s) != 0;)
        {
            grpc::Slice s{ grpc_s, grpc::Slice::STEAL_REF };
            std::memcpy(dest, s.begin(), s.size());
            dest += s.size();
        }

        BOOST_ASSERT(dest == buff.get() + bufferSize);

        grpc_byte_buffer_reader_destroy(&reader);
        grpc_byte_buffer_destroy(buffer);

        // TODO: create a Bond input stream over grpc::ByteBuffer to avoid
        // having to make this copy into a blob.
        blob data(buff, static_cast<uint32_t>(bufferSize));
        CompactBinaryReader<InputBuffer> cbreader(data);
        msg = bonded<T>(cbreader);

        return grpc::Status::OK;
    }

} } } } //namespace bond::ext::gRPC::detail

namespace grpc
{
    template <class T>
    class SerializationTraits<bond::bonded<T>, typename std::enable_if<bond::is_bond_type<T>::value>::type>
    {
    public:
        static Status Serialize(const bond::bonded<T>& msg, ByteBuffer* buffer, bool* own_buffer)
        {
            BOOST_ASSERT(buffer);
            BOOST_ASSERT(own_buffer);

            return bond::ext::gRPC::detail::Serialize(msg, *buffer, *own_buffer);
        }

        // TODO: Use grpc::ByteBuffer after https://github.com/grpc/grpc/issues/14880 is fixed.
        static Status Deserialize(grpc_byte_buffer* buffer, bond::bonded<T>* msg)
        {
            BOOST_ASSERT(msg);

            if (!buffer)
            {
                return { StatusCode::INTERNAL, "No payload" };
            }

            return bond::ext::gRPC::detail::Deserialize(buffer, *msg);
        }
    };

}  // namespace grpc
