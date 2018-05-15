// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bonded.h>
#include <bond/ext/grpc/exception.h>
#include <bond/ext/grpc/client_callback.h>

#include <grpcpp/impl/codegen/status.h>

#include <boost/optional.hpp>

#include <chrono>
#include <condition_variable>
#include <memory>
#include <mutex>


namespace bond { namespace ext { namespace gRPC {

/// @brief A callback type that can be manually waited upon.
///
/// The type can be used to synchronously get the result of invoking an
/// async proxy method.
///
/// The wait() member function can be used to wait until the callback has
/// been called. Then, the status() and response() member functions can be
/// called to inspect the results.
template <typename Response>
class wait_callback final
{
public:
    using arg_type = unary_call_result<Response>;

    /// @brief Records the response and status.
    ///
    /// @exception MultipleInvocationException thrown if the callback (or a
    /// copy of the callback) is invoked more than once.
    void operator()(arg_type result) const
    {
        {
            std::lock_guard<std::mutex> lock(*_state);

            if (!_state->result)
            {
                _state->result = std::move(result);
            }
            else
            {
                throw MultipleInvocationException();
            }
        }

        _state->notify_all();
    }

    /// @brief Waits for this to have been invoked.
    void wait() const
    {
        std::unique_lock<std::mutex> lock(*_state);
        _state->wait(lock, [this] { return static_cast<bool>(_state->result); });
    }

    /// @brief Waits at least \p timeout for this to have been invoked.
    ///
    /// @param timeout the minimum amount of time to wait.
    ///
    /// @return \p true if a callback was invoked. \p false if the timeout
    /// occured.
    template <typename Rep, typename Period>
    bool wait_for(const std::chrono::duration<Rep, Period>& timeout) const
    {
        std::unique_lock<std::mutex> lock(*_state);
        return _state->wait_for(lock, timeout, [this] { return static_cast<bool>(_state->result); });
    }

    /// @brief Gets the response.
    ///
    /// @warning Blocks until this has been invoked.
    const bonded<Response>& response() const
    {
        wait();
        return _state->result->response();
    }

    /// @brief Gets the status.
    ///
    /// @warning Blocks until this has been invoked.
    const grpc::Status& status() const
    {
        wait();
        return _state->result->status();
    }

    /// @brief Gets the context.
    ///
    /// @warning Blocks until this has been invoked.
    const std::shared_ptr<grpc::ClientContext>& context() const
    {
        wait();
        return _state->result->context();
    }

private:
    struct state : std::mutex, std::condition_variable
    {
        boost::optional<arg_type> result;
    };

    std::shared_ptr<state> _state{ std::make_shared<state>() };
};

/// @example wait_callback_example.cpp
///
/// This is a brief example showing how wait_callback can be used to
/// synchronously get the result of invoking an async proxy method.

} } } // bond::ext::gRPC
