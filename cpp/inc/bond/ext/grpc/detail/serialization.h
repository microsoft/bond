// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bond.h>
#include <bond/ext/grpc/exception.h>
#include <bond/stream/output_buffer.h>

#include <grpcpp/support/byte_buffer.h>

#include <boost/container/small_vector.hpp>
#include <boost/intrusive_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/smart_ptr/intrusive_ref_counter.hpp>

namespace bond { namespace ext { namespace grpc { namespace detail
{
    /// @brief A helper wrapper to fix crashing copy .ctor in ::grpc::ByteBuffer
    //
    // After https://github.com/grpc/grpc/pull/15517 is merged and released, we can
    // retire this class.
    class ByteBuffer
    {
    public:
        ByteBuffer() = default;

        ByteBuffer(const ::grpc::ByteBuffer& buffer)
        {
            _buffer = buffer;   // The operator= is fine.
        }

        ByteBuffer(const ByteBuffer& other)
            : ByteBuffer{ other._buffer }
        {}

        ::grpc::ByteBuffer* operator&()
        {
            return &_buffer;
        }

        operator ::grpc::ByteBuffer&()
        {
            return _buffer;
        }

        operator const ::grpc::ByteBuffer&() const
        {
            return _buffer;
        }

    private:
        ::grpc::ByteBuffer _buffer;
    };

    inline ::grpc::ByteBuffer to_byte_buffer(const OutputBuffer& output)
    {
        struct Buffers
            : boost::intrusive_ref_counter<Buffers>,
              boost::container::small_vector<blob, 8>
        {};

        boost::intrusive_ptr<Buffers> buffers{ new Buffers };
        output.GetBuffers(*buffers);

        boost::container::small_vector<::grpc::Slice, 8> slices;
        slices.reserve(buffers->size());

        for (blob& data : *buffers)
        {
            data = blob_prolong(std::move(data));

            slices.emplace_back(
                const_cast<void*>(data.data()), // The buffer is not expected to be modified, but
                                                // we have to const_cast because ::grpc::Slice ctor
                                                // only takes void*.
                data.size(),
                [](void* arg) { intrusive_ptr_release(static_cast<Buffers*>(arg)); },
                buffers.get());

            intrusive_ptr_add_ref(buffers.get());
        }

        return ::grpc::ByteBuffer{ slices.data(), slices.size() };
    }

    template <typename T>
    inline ::grpc::ByteBuffer Serialize(const bonded<T>& msg)
    {
        OutputBuffer output;
        CompactBinaryWriter<OutputBuffer> writer(output);

        msg.Serialize(writer);

        return to_byte_buffer(output);
    }

    inline InputBuffer from_byte_buffer(const ::grpc::ByteBuffer& buffer)
    {
        std::vector<::grpc::Slice> slices;

        auto status = buffer.Dump(&slices);
        if (!status.ok())
        {
            throw GrpcException{ status };
        }

        const auto length = buffer.Length();
        auto buff = boost::make_shared_noinit<char[]>(length);

        char* dest = buff.get();
        for (auto& s : slices)
        {
            std::memcpy(dest, s.begin(), s.size());
            dest += s.size();
        }

        // TODO: create a Bond input stream over ::grpc::ByteBuffer to avoid
        // having to make this copy into a blob.
        blob data{ buff, static_cast<uint32_t>(length) };
        return InputBuffer{ data };
    }

    template <typename T>
    inline bonded<T> Deserialize(const ::grpc::ByteBuffer& buffer)
    {
        return bonded<T>{ CompactBinaryReader<InputBuffer>{ from_byte_buffer(buffer) } };
    }

} } } } //namespace bond::ext::grpc::detail
