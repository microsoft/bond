// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/traits.h>
#include <bond/stream/stream_interface.h>

namespace bond
{
namespace detail
{

// Fast pass-through is implemented by calling protocol reader to skip the value
// and then writing the skipped data as a single blob to protocol writer.
template <typename T, typename Reader, typename Writer>
void PassThrough(bonded<T, Reader&>& value, Reader& reader, Writer& writer)
{
    BOOST_STATIC_ASSERT((is_protocol_same<Reader, Writer>::value));

    auto before = GetCurrentBuffer(reader.GetBuffer());

    value.Skip();

    auto after = GetCurrentBuffer(reader.GetBuffer());

    writer.GetBuffer().Write(GetBufferRange(before, after));
}

}
}
