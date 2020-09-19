// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/blob.h>
#include <bond/core/detail/mpl.h>

#include <boost/static_assert.hpp>

#include <type_traits>

namespace bond
{

//
// input stream concept
//

#if 0
class InputStream
{
public:
    // Read overload(s) for arithmetic types
    template <typename T>
    void Read(T& value);

    // Read into a memory buffer
    void Read(void *buffer, uint32_t size);

    // Read into a memory blob
    void Read(bond::blob& blob, uint32_t size);

    // Skip specified amount of bytes
    void Skip(std::uint32_t size);

    // Check if EOF is reached
    bool IsEof() const;
};
#endif


//
// input stream functions - overload for custom streams
//


// Returns an object that represents the input stream's current position.
// Return type is not required to be specifically bond::blob.
// See GetBufferRange for more details.
template <typename InputBuffer>
[[noreturn]] inline blob GetCurrentBuffer(const InputBuffer& /*input*/)
{
    BOOST_STATIC_ASSERT_MSG(
        detail::mpl::always_false<InputBuffer>::value,
        "GetCurrentBuffer is undefined.");
}


// Returns an object that represents a buffer range. The input arguments are
// determined by what the GetCurrentBuffer returns for the given input buffer
// implementation (i.e. not necessarily a blob). The GetBufferRange may return
// a type different from blob as long as it is understood by the corresponding
// output buffer associated with the writer (i.e. there is an overloaded Write
// function that accepts it).
template <typename Blob>
[[noreturn]] inline Blob GetBufferRange(const Blob& /*begin*/, const Blob& /*end*/)
{
    BOOST_STATIC_ASSERT_MSG(
        detail::mpl::always_false<Blob>::value,
        "GetBufferRange is undefined.");
}


//
// output stream concept
//

#if 0
class OutputStream
{
public:
    // Write overload(s) for arithmetic types
    template<typename T>
    void Write(const T& value);

    // Write a memory buffer
    void Write(const void* value, uint32_t size);

    // Write a memory blob
    void Write(const bond::blob& blob);
};
#endif


//
// output stream functions - overload for custom streams
//


// Creates a new output buffer based on an existing one. The result does
// not need to be the same type. The function is used in the places
// where a new output buffer needs to be constructed with similar and/or
// compatible properties as the original one (e.g. while serializing
// bonded<T> for untagged protocols).
template <typename OutputBuffer>
[[noreturn]] inline OutputBuffer CreateOutputBuffer(const OutputBuffer& /*other*/)
{
    BOOST_STATIC_ASSERT_MSG(
        detail::mpl::always_false<OutputBuffer>::value,
        "CreateOutputBuffer is undefined.");
}


}
