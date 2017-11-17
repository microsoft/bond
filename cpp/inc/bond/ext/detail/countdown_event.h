// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "event.h"

#include <boost/assert.hpp>

#include <atomic>
#include <chrono>

namespace bond { namespace ext { namespace detail {

/// @ A synchronization primitive that is signaled when its count reaches
/// zero.
class countdown_event final
{
public:
    /// @param count the number of times the event must be set before it is
    /// signaled. Must be positive.
    explicit countdown_event(size_t count)
        : _e(),
        _cnt(count)
    {
        BOOST_ASSERT(count > 0);
    }

    /// @brief Decrements the count, potentially signalling the event.
    ///
    /// @warning If \ref set is called more times that the initial count,
    /// the behavior is undefined.
    ///
    /// @return \p true if this signal caused the event to be signaled,
    /// otherwise \p false.
    bool set()
    {
        size_t newValue = --_cnt;

        if (newValue == 0)
        {
            _e.set();
            return true;
        }

        return false;
    }

    /// @brief Wait for the count to reach zero.
    void wait()
    {
        _e.wait();
    }

    /// @brief Waits at least \p timeout for the count to reach zero.
    ///
    /// @param timeout the minimum amount of time to wait for the event to
    /// enter the signaled state.
    ///
    /// @return \p true if the event was signaled. \p false if the timeout
    /// occured.
    template <typename Rep, typename Period>
    bool wait_for(const std::chrono::duration<Rep, Period>& timeout)
    {
        return _e.wait_for(timeout);
    }

private:
    event _e;
    std::atomic<size_t> _cnt;

    countdown_event(const countdown_event&) = delete;
    countdown_event(countdown_event&&) = delete;
    countdown_event& operator=(const countdown_event&) = delete;
    countdown_event& operator=(countdown_event&&) = delete;
};

} } } // namespace bond::ext::detail
