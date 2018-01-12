// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/static_assert.hpp>
#include <boost/utility/enable_if.hpp>

#include <cstdint>
#include <limits>
#include <stdexcept>
#include <type_traits>

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
    inline typename boost::enable_if_c<std::is_unsigned<T>::value && std::is_unsigned<U>::value, T>::type
    checked_add(T lhs, U rhs)
    {
        BOOST_STATIC_ASSERT(sizeof(T) >= sizeof(U));

        if (((std::numeric_limits<T>::max)() - lhs) < rhs)
        {
            throw std::overflow_error("Overflow on addition");
        }
        return lhs + rhs;
    }

    template <typename T>
    inline typename boost::enable_if_c<std::is_unsigned<T>::value && (sizeof(T) < sizeof(uint64_t)), T>::type
    checked_multiply(T lhs, uint8_t rhs)
    {
        uint64_t result = static_cast<uint64_t>(lhs) * static_cast<uint64_t>(rhs);

        if (result > (std::numeric_limits<T>::max)())
        {
            throw std::overflow_error("Overflow on multiplication");
        }

        return static_cast<T>(result);
    }

} // namespace detail
} // namespace bond

