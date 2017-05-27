// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/bonded.h>
#include <grpc++/impl/codegen/status.h>

#include <boost/optional.hpp>

#include <chrono>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <tuple>

namespace bond { namespace ext { namespace gRPC {

/// @brief A callback type that can be manually waited upon.
///
/// The type can be used to synchronously get the result of invoking an
/// async proxy method.
///
/// The wait() member function can be used to wait until the callback has
/// been called. Then, the status() and response() member functions can be
/// called to inspect the results.
template <typename TResponse>
class wait_callback
{
public:
    wait_callback() : _impl(std::make_shared<impl>()) { }

    /// @brief Records the response and status.
    ///
    /// May be invoked multiple times and from different threads, but only the first
    /// invocation will be recorded.
    void operator()(const bond::bonded<TResponse>& response, const grpc::Status& status)
    {
        std::unique_lock<std::mutex> lock(_impl->_m);
        if (!_impl->_results)
        {
            _impl->_results.emplace(response, status);

            // Drop the lock before notifying so we don't wake someone up to
            // then have them wait on the lock.
            lock.unlock();
            _impl->_cv.notify_all();
        }
    }

    /// @brief Waits for this to have been invoked.
    void wait() const
    {
        std::unique_lock<std::mutex> lock(_impl->_m);
        _impl->_cv.wait(lock, [this]() { return static_cast<bool>(_impl->_results); });
    }

    /// @brief Waits at least \p timeout for this to have been invoked.
    ///
    /// @param timeout the minimum amount of time to wait.
    ///
    /// @return \p true if a callback was invoked. \p false if the timeout
    /// occured.
    template <typename Rep, typename Period>
    bool wait(const std::chrono::duration<Rep, Period>& timeout) const
    {
        std::unique_lock<std::mutex> lock(_impl->_m);
        return _impl->_cv.wait_for(lock, timeout, [this]() { return static_cast<bool>(_impl->_results); });
    }

    /// @brief Gets the response.
    ///
    /// @warning Blocks until this has been invoked.
    const bond::bonded<TResponse>& response() const
    {
        wait();
        return std::get<0>(_impl->_results.get());
    }

    /// @brief Gets the status.
    ///
    /// @warning Blocks until this has been invoked.
    const grpc::Status& status() const
    {
        wait();
        return std::get<1>(_impl->_results.get());
    }

private:
    /// The interesting guts of wait_callback. We use an impl class so that
    /// wait_callback can be copied and all the copies affect the same underlying
    /// state.
    struct impl
    {
        impl() = default;
        impl(const impl&) = delete;
        impl(impl&&) = delete;
        impl& operator=(const impl&) = delete;
        impl& operator=(impl&&) = delete;

        /// mutex to lock the shared state
        mutable std::mutex _m;
        /// condition variable used to signal anyone waiting
        mutable std::condition_variable _cv;
        /// The response and status, but more importantly, doubles as a flag
        /// indicating whether a callback has been invoked yet. If this is
        /// empty, no callback has been invoked yet. If non-empty, a
        /// callback has already been invoked.
        boost::optional<std::tuple<bond::bonded<TResponse>, grpc::Status>> _results;
    };

    /// shared_ptr to the actual state.
    std::shared_ptr<impl> _impl;
};

/// @example wait_callback_example.cpp
///
/// This is a brief example showing how wait_callback can be used to
/// synchronously get the result of invoking an async proxy method.

} } } // bond::extgrpc
