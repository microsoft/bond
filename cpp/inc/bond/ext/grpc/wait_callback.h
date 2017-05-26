// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/bonded.h>
#include <grpc++/impl/codegen/status.h>

#include <boost/optional.hpp>

#include <chrono>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>

namespace bond { namespace ext { namespace gRPC {

/// @brief A callback type that can be manually waited upon.
///
/// The \ref wait member function can be used to wait until the callback
/// has been called. Then, the \ref status and \ref response member
/// functions can be called to inspect the results.
///
/// @example wait_callback.cpp
template <typename TResponse>
class wait_callback
{
public:
    wait_callback() : _impl(std::make_shared<impl>()) { }

    using CallbackType = std::function<void(const bond::bonded<TResponse>&, const grpc::Status&)>;

    /// @brief Creates a callback that will signal this wait_callback
    /// instance.
    ///
    /// If multiple callbacks are created and invoked, only the results of
    /// the first one that is invoked will be recorded.
    ///
    /// It is safe for callbacks to outlive the wait_callback that created
    /// them. When they are invoked their results will be discarded.
    CallbackType callback()
    {
        // Create a weak_ptr to the implementation for the callback. If all
        // the wait_callback instances sharing the implementation go away,
        // their callbacks can still safely be invoked: they just won't be
        // able to lock the weak_ptr and thus won't do anything.
        std::weak_ptr<impl> wpImpl(_impl);

        return std::function<void(const bond::bonded<TResponse>&, const grpc::Status&)>(
            [wpImpl](const bond::bonded<TResponse>& response, const grpc::Status& status)
            {
                std::shared_ptr<impl> impl = wpImpl.lock();

                if (impl)
                {
                    impl->set(response, status);
                }
            });
    }

    /// @brief Implicitly creates a callback.
    operator CallbackType()
    {
        return callback();
    }

    /// @brief Waits for a callback created by \ref callback to be
    /// invoked.
    void wait() const
    {
        _impl->wait();
    }

    /// @brief Waits at least \p timeout for a callback created by \ref
    /// callback to be invoked.
    ///
    /// @param timeout the minimum amount of time to wait.
    ///
    /// @return \p true if a callback was invoked. \p false if the timeout
    /// occured.
    template <typename Rep, typename Period>
    bool wait(std::chrono::duration<Rep, Period> timeout) const
    {
        return _impl->wait(timeout);
    }

    /// @brief Gets the response.
    ///
    /// @warning Blocks until a callback is invoked.
    const bond::bonded<TResponse>& response() const
    {
        return _impl->response();
    }

    /// @brief Gets the status.
    ///
    /// @warning Blocks until a callback is invoked.
    const grpc::Status& status() const
    {
        return _impl->status();
    }

private:
    /// The interesting guts of wait_callback. We use an impl class so that
    /// we can handout weak_ptr instances to be captured by the callbacks
    /// without forcing clients of wait_callback to deal with creating a
    /// shared_ptr.
    struct impl
    {
        /// mutex to lock the shared state
        mutable std::mutex _m;
        /// condition variable used to signal anyone waiting
        mutable std::condition_variable _cv;
        /// The response, but more importantly, doubles as a flag indicating
        /// whether a callback has been invoked yet. If this is empty, no
        /// callback has been invoked yet. If non-empty, a callback has
        /// already been invoked.
        boost::optional<bond::bonded<TResponse>> _response;
        /// The status.
        boost::optional<grpc::Status> _status;

        impl() = default;
        impl(const impl&) = delete;
        impl(impl&&) = delete;
        impl& operator=(const impl&) = delete;
        impl& operator=(impl&&) = delete;

        /// Attempts to set \ref _response and \ref _status. Only the first
        /// call to set that completes will actuall set \ref _response and
        /// \ref _status. The rest will do nothing.
        void set(const bond::bonded<TResponse>& response, const grpc::Status& status)
        {
            std::unique_lock<std::mutex> lock(_m);
            if (!_response)
            {
                _response.emplace(response);
                _status.emplace(status);
                lock.unlock();
                _cv.notify_all();
            }
        }

        void wait() const
        {
            std::unique_lock<std::mutex> lock(_m);
            return _cv.wait(lock, [this]() { return static_cast<bool>(_response); });
        }

        template <typename Rep, typename Period>
        bool wait(std::chrono::duration<Rep, Period> timeout) const
        {
            std::unique_lock<std::mutex> lock(_m);
            return _cv.wait_for(lock, timeout, [this]() { return static_cast<bool>(_response); });
        }

        const bond::bonded<TResponse>& response() const
        {
            wait();
            return _response.get();
        }

        const grpc::Status& status() const
        {
            wait();
            return _status.get();
        }
    };

    /// shared_ptr to the actual implementation and storage. Used so the
    /// created callbacks can be given a weak_ptr to the implementation.
    std::shared_ptr<impl> _impl;
};

} } } // bond::extgrpc
