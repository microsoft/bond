// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "capped_allocator_fwd.h"
#include "detail/counter_base.h"
#include <boost/assert.hpp>


namespace bond
{
    /// @brief Single-threaded counter to be used with \ref capped_allocator.
    ///
    /// @tparam T underlying counter type.
    template <typename T>
    class single_threaded_counter : public detail::counter_base<T>
    {
    public:
        using is_thread_safe = std::false_type;

#if !defined(_MSC_VER) || _MSC_VER >= 1900
        using detail::counter_base<T>::counter_base;
#else
        explicit single_threaded_counter(T max_value) BOND_NOEXCEPT
            : detail::counter_base<T>(max_value)
        {}
#endif

        bool try_add(T n) BOND_NOEXCEPT
        {
            if (n <= this->max_value() && _value <= this->max_value() - n)
            {
                _value += n;
                return true;
            }

            return false;
        }

        void subtract(T n) BOND_NOEXCEPT
        {
            BOOST_ASSERT(_value >= n);
            _value -= n;
        }

        T value() const BOND_NOEXCEPT
        {
            return _value;
        }

    private:
        T _value{};
    };

} // namespace bond
