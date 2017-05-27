// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/assert.hpp>

#include <chrono>
#include <condition_variable>
#include <mutex>

namespace bond { namespace ext { namespace detail {

    /// @brief A synchronization primitive that is signaled manually.
    ///
    /// @remarks This class only provides a \ref set method. It
    /// intentionally does not have a \p reset method to avoid the potential
    /// for waiters missing a signal if \p set and \p reset were to be
    /// called in quick succession.
    class event final
    {
    public:
        explicit event()
            : _m(),
              _cv(),
             _isSet(false)
        {}

        /// @brief Signals the event, releasing current and future waiters.
        void set()
        {
            {
                std::lock_guard<std::mutex> lock(_m);
                _isSet = true;
            }

            _cv.notify_all();
        }

        /// @brief Waits for the event to enter the signaled state.
        void wait()
        {
            std::unique_lock<std::mutex> lock(_m);
            return _cv.wait(lock, [this]() {return _isSet;});
        }

        /// @brief Waits at least \p timeout for the event to enter the
        /// signaled state.
        ///
        /// @param timeout the minimum amount of time to wait for the event
        /// to enter the signaled state.
        ///
        /// @return \p true if the event was signaled. \p false if the
        /// timeout occured.
        template <typename Rep, typename Period>
        bool wait(const std::chrono::duration<Rep, Period>& timeout)
        {
            std::unique_lock<std::mutex> lock(_m);
            return _cv.wait_for(lock, timeout, [this]() {return _isSet;});
        }

    private:
        std::mutex _m;
        std::condition_variable _cv;
        bool _isSet;

        event(const event&) = delete;
        event(event&&) = delete;
        event& operator=(const event&) = delete;
        event& operator=(event&&) = delete;
    };

} } } // namespace bond::ext::detail
