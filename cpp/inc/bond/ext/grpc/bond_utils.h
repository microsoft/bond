// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bond.h>
#include <bond/core/bonded.h>
#include <bond/core/reflection.h>
#include <bond/stream/output_buffer.h>

#include <grpc/impl/codegen/byte_buffer_reader.h>
#include <grpc/impl/codegen/slice.h>
#include <grpcpp/impl/codegen/config.h>
#include <grpcpp/impl/codegen/core_codegen.h>
#include <grpcpp/impl/codegen/core_codegen_interface.h>
#include <grpcpp/impl/codegen/serialization_traits.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/status_code_enum.h>
#include <grpcpp/support/slice.h>

#include <boost/assert.hpp>
#include <boost/make_shared.hpp>

#include <cstdint>
#include <cstdlib>
#include <limits>
#include <stdint.h>

namespace grpc {

    template <class T>
    class SerializationTraits<bond::bonded<T>, typename std::enable_if<bond::is_bond_type<T>::value>::type>
    {
    public:
        static Status Serialize(
            const bond::bonded<T>& msg,
            grpc_byte_buffer** bp,
            bool* own_buffer)
        {
            *own_buffer = true;

            bond::OutputBuffer output;
            bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

            msg.Serialize(writer);

            bond::blob data = output.GetBuffer();

            grpc_slice slice = grpc_slice_from_copied_buffer(data.content(), data.length());

            *bp = grpc_raw_byte_buffer_create(&slice, 1);

            grpc_slice_unref(slice);

            return Status::OK;
        }

        static Status Deserialize(grpc_byte_buffer* buffer, bond::bonded<T>* msg)
        {
            if (!buffer)
            {
                return Status(StatusCode::INTERNAL, "No payload");
            }

            const size_t bufferSize = grpc_byte_buffer_length(buffer);

            if (bufferSize > (std::numeric_limits<uint32_t>::max)())
            {
                grpc_byte_buffer_destroy(buffer);
                return Status(StatusCode::INTERNAL, "Buffer is too large");
            }

            boost::shared_ptr<char[]> buff = boost::make_shared_noinit<char[]>(bufferSize);

            // TODO: exception safety of reader, buffer
            grpc_byte_buffer_reader reader;
            if (!grpc_byte_buffer_reader_init(&reader, buffer))
            {
                grpc_byte_buffer_destroy(buffer);
                return Status(StatusCode::INTERNAL, "Failed to init buffer reader");
            }

            char* dest = buff.get();

            for (grpc_slice grpc_s; grpc_byte_buffer_reader_next(&reader, &grpc_s) != 0;)
            {
                grpc::Slice s{ grpc_s, grpc::Slice::STEAL_REF };
                std::memcpy(dest, s.begin(), s.size());
                dest += s.size();
            }

            BOOST_ASSERT(dest == buff.get() + bufferSize);

            grpc_byte_buffer_reader_destroy(&reader);
            grpc_byte_buffer_destroy(buffer);

            // TODO: create a Bond input stream over grpc_byte_buffer to avoid
            // having to make this copy into a blob.
            bond::blob data = bond::blob(buff, static_cast<uint32_t>(bufferSize));
            bond::CompactBinaryReader<bond::InputBuffer> cbreader(data);
            *msg = bond::bonded<T>(cbreader);

            return Status::OK;
        }
    };

}  // namespace grpc
