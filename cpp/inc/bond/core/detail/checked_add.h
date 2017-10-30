// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <cstdint>
#include <limits>
#include <stdexcept>
#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>
#include <boost/utility.hpp>

namespace bond
{
namespace detail
{

    inline const char* checked_add(const char* ptr, uint32_t offset)
    {
        // Numeric limit logic below requires uintptr_t to be exactly same size as a pointer
        BOOST_STATIC_ASSERT(sizeof(const char*) == sizeof(std::uintptr_t));

        std::uintptr_t uintptr = reinterpret_cast<std::uintptr_t>(ptr);
        if (((std::numeric_limits<uintptr_t>::max)() - offset) < uintptr)
        {
            throw std::overflow_error("Offset caused pointer to overflow");
        }
        return ptr + offset;
    }

    template <typename T, typename U>
    typename boost::enable_if_c<boost::is_integral<T>::value && boost::is_unsigned<T>::value
                                && boost::is_integral<U>::value && boost::is_unsigned<U>::value, T>::type
    checked_add(T lhs, U rhs)
    {
        BOOST_STATIC_ASSERT(sizeof(T) >= sizeof(U));

        if (((std::numeric_limits<T>::max)() - lhs) < rhs)
        {
            throw std::overflow_error("Overflow on addition");
        }
        return lhs + rhs;
    }

} // namespace detail
} // namespace bond

