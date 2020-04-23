// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/protocol/compact_binary.h>
#include <bond/stream/stream_interface.h>

namespace bond
{
namespace detail
{

template <typename Reader>
auto ReadBlob(Reader& reader)
#if defined(BOND_NO_CXX14_RETURN_TYPE_DEDUCTION)
    -> decltype(GetBufferRange(GetCurrentBuffer(reader.GetBuffer()), GetCurrentBuffer(reader.GetBuffer())))
#endif
{
    uint32_t size;
    reader.Read(size);

    auto before = GetCurrentBuffer(reader.GetBuffer());
    reader.GetBuffer().Skip(size);
    auto after = GetCurrentBuffer(reader.GetBuffer());

    return GetBufferRange(before, after);
}


template <typename Protocols, typename T, typename Writer>
void MarshalToBlob(const T& obj, Writer& writer)
{
    auto output = CreateOutputBuffer(writer.GetBuffer());
    CompactBinaryWriter<decltype(output)> cbw(output);

    Marshal<Protocols>(obj, cbw);
    auto data = std::move(output).GetBuffer();

    writer.Write(static_cast<uint32_t>(data.size()));
    writer.GetBuffer().Write(data);
}


} // namespace detail
} // namespace bond
