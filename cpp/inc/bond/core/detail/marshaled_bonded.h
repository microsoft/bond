// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/stream/output_buffer.h>

namespace bond
{
namespace detail
{

template <typename Reader>
blob ReadBlob(Reader& reader)
{
    uint32_t size;
    blob buffer;

    reader.Read(size);
    reader.Read(buffer, size);
    return buffer;
}


template <typename T, typename Writer>
void MarshalToBlob(const T& obj, Writer& writer)
{
    OutputBuffer output;
    CompactBinaryWriter<OutputBuffer> cbw(output);

    Marshal(obj, cbw);
    blob data = output.GetBuffer();

    writer.Write(data.size());
    writer.Write(data);
}


} // namespace detail
} // namespace bond
