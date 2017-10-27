// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <grpc++/impl/codegen/config.h>
#include <grpc++/impl/codegen/core_codegen.h>
#include <grpc++/impl/codegen/core_codegen_interface.h>
#include <grpc++/impl/codegen/serialization_traits.h>
#include <grpc++/impl/codegen/status.h>
#include <grpc++/impl/codegen/status_code_enum.h>
#include <grpc/impl/codegen/byte_buffer_reader.h>
#include <grpc/impl/codegen/slice.h>

#include <bond/core/bond.h>
#include <bond/core/bonded.h>
#include <bond/core/reflection.h>
#include <bond/stream/output_buffer.h>

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

            // TODO: exception safety of reader, s, buffer
            grpc_byte_buffer_reader reader;
            if (!grpc_byte_buffer_reader_init(&reader, buffer))
            {
                grpc_byte_buffer_destroy(buffer);
                return Status(StatusCode::INTERNAL, "Failed to init buffer reader");
            }

            char* dest = buff.get();

            grpc_slice s;
            while (grpc_byte_buffer_reader_next(&reader, &s) != 0)
            {
                std::memcpy(dest, GRPC_SLICE_START_PTR(s), GRPC_SLICE_LENGTH(s));
                dest += GRPC_SLICE_LENGTH(s);
                grpc_slice_unref(s);
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
