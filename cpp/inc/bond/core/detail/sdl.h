// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/* Microsoft publishes the Security Development Lifecycle (SDL) which "is a
 * software development process that helps developers build more secure
 * software and address security compliance requirements while reducing
 * development cost."
 *
 * Some more details at https://www.microsoft.com/en-us/sdl/
 *
 * The code in this file allows Bond to access features in MSVC that are
 * specific to the SDL while maintaining Bond's cross-compiler and
 * cross-platform compability.
 */

#pragma once

#include <bond/core/config.h>

#include <boost/assert.hpp>
#include <boost/core/ignore_unused.hpp>
#include <iterator>

namespace bond { namespace detail
{

#ifdef _MSC_VER

template<class Iterator> inline
stdext::checked_array_iterator<Iterator> make_checked_array_iterator(
    Iterator array,
    size_t size,
    size_t index = 0)
{
    // Allows algorithms like std::copy to work on pointers by encoding
    // bounds information.
    return stdext::checked_array_iterator<Iterator>(array, size, index);
}

#else

template<class Iterator> inline
Iterator make_checked_array_iterator(
    Iterator array,
    size_t size,
    size_t index  = 0)
{
    boost::ignore_unused(size);
    BOOST_ASSERT(index <= size);

    return array + index;
}

#endif

}}
