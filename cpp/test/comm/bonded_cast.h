
#pragma once

#include <bond/core/bonded.h>
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/stream/input_buffer.h>

namespace bond { namespace comm
{

//
// Helper function.
//
template <typename T, typename U>
inline
bonded<T> bonded_cast(const U& value,
                      uint16_t version = default_version<CompactBinaryReader<InputBuffer> >::value)
{
    OutputBuffer buffer;

    CompactBinaryWriter<OutputBuffer> writer(buffer, version);
    Serialize(value, writer);

    return bonded<U>(
        CompactBinaryReader<InputBuffer>(buffer.GetBuffer(), version));
}

} } // namespace bond::comm
