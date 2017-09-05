// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <limits>
#include <stdexcept>

namespace bond
{
namespace detail
{

    inline void check_add_overflow(const char* ptr, uint32_t offset)
    {
        if ((ptr + offset) < ptr)
        {
            throw std::overflow_error("Overflow on addition");
        }
    }

    inline void check_add_overflow(uint32_t lhs, uint32_t rhs)
    {
        if (((std::numeric_limits<uint32_t>::max)() - lhs) < rhs)
        {
            throw std::overflow_error("Overflow on addition");
        }
    }

} // namespace detail
} // namespace bond

