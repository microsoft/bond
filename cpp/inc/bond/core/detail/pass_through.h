// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/traits.h>
#include <bond/core/blob.h>

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

    blob before = GetCurrentBuffer(reader.GetBuffer());

    value.Skip();

    blob after = GetCurrentBuffer(reader.GetBuffer());

    blob data;

    data.assign(before, 0, before.length() - after.length());

    WriteRawBlob(writer, data);
}

}
}
