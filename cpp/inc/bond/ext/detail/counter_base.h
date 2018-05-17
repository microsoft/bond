// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/static_assert.hpp>

#include <type_traits>

namespace bond { namespace ext { namespace detail
{
    /// @brief Helper base class for counters.
    template <typename T>
    class counter_base
    {
    public:
        BOOST_STATIC_ASSERT(std::is_integral<T>::value);

        using value_type = T;

        explicit counter_base(T max_value) BOND_NOEXCEPT
            : _max_value{ max_value }
        {}

        counter_base(const counter_base& other) = delete;

        T max_value() const BOND_NOEXCEPT
        {
            return _max_value;
        }

    private:
        const T _max_value;
    };

} } } // namespace bond::ext::detail
