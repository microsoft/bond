// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "capped_allocator_fwd.h"
#include "detail/counter_base.h"

#include <boost/assert.hpp>

#include <atomic>


namespace bond { namespace ext
{
    /// @brief Multi-threaded counter to be used with \ref capped_allocator.
    ///
    /// @tparam T underlying counter type.
    template <typename T>
    class multi_threaded_counter : public detail::counter_base<T>
    {
    public:
        using is_thread_safe = std::true_type;

        using detail::counter_base<T>::counter_base;

        bool try_add(T n) BOND_NOEXCEPT
        {
            if (n <= this->max_value())
            {
                const auto max_val = this->max_value() - n;

                for (auto val = _value.load(std::memory_order::memory_order_acquire); val <= max_val; )
                {
                    if (_value.compare_exchange_weak(
                            val,
                            val + n,
                            std::memory_order::memory_order_release,
                            std::memory_order::memory_order_acquire))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        void subtract(T n) BOND_NOEXCEPT
        {
            BOOST_ASSERT(value() >= n);
            _value.fetch_sub(n, std::memory_order::memory_order_relaxed);
        }

        /// @remarks The returned value may not be up-to-date.
        T value() const BOND_NOEXCEPT
        {
            return _value.load(std::memory_order::memory_order_relaxed);
        }

    private:
        std::atomic<T> _value{};
    };

} } // namespace bond::ext
