// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "event.h"

#include <boost/assert.hpp>

#include <atomic>
#include <chrono>

namespace bond { namespace ext { namespace detail {

/// @brief A synchronization primitive that is signaled when a certain
/// number of threads are waiting on it at the same time.
///
/// A barrier can only be used once: there is no way to reset it, even after
/// all the threads have entered.
class barrier final
{
public:
    /// @param count the number of threads that must enter before they will
    /// all be released. Must be positive.
    explicit barrier(size_t count)
        : _e(),
        _cnt(count)
    {
        BOOST_ASSERT(count > 0);
    }

    /// @brief Enter the barrier, decrement the count, and potentially
    /// release the other waiting threads.
    ///
    /// @warning If more threads enter the barrier than its \p count, the
    /// behavior is undefined.
    ///
    /// @return \p true for exactly one thread, \p false for the rest.
    bool enter()
    {
        size_t newValue = --_cnt;

        if (newValue == 0)
        {
            _e.set();
            return true;
        }
        else
        {
            _e.wait();
            return false;
        }
    }

    /// @brief Wait for all the threads to enter the barrier.
    ///
    /// @note This is intended for "observer" threads. It doesn't affect the
    /// count and will never release any other threads.
    void wait()
    {
        _e.wait();
    }

    /// @brief Waits at least \p timeout for all the threads to enter the
    /// barrier.
    ///
    /// @note This is intended for "observer" threads. It doesn't affect the
    /// count and will never release any other threads.
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

    barrier(const barrier&) = delete;
    barrier(barrier&&) = delete;
    barrier& operator=(const barrier&) = delete;
    barrier& operator=(barrier&&) = delete;
};

} } } // namespace bond::ext::detail
