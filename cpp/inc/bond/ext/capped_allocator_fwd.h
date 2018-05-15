// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <cstdint>
#include <memory>

namespace bond { namespace ext
{
    template <typename T = std::size_t>
    class single_threaded_counter;

    template <typename T = std::size_t>
    class multi_threaded_counter;

    template <typename Counter = multi_threaded_counter<>>
    class shared_counter;

    template <
        typename Alloc = std::allocator<char>,
        typename Counter = shared_counter<
            multi_threaded_counter<typename std::allocator_traits<Alloc>::size_type>>>
    class capped_allocator;

} } // namespace bond::ext
