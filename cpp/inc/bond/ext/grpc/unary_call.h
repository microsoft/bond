// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/ext/grpc/detail/unary_call_impl.h>

#include <boost/smart_ptr/intrusive_ptr.hpp>

#include <utility>

namespace bond { namespace ext { namespace gRPC {

namespace detail {

    template <typename TRequest, typename TResponse, typename TThreadPool>
    class service_unary_call_data;

} // namespace detail

template <typename TRequest, typename TResponse>
class shared_unary_call;

/// @brief Exclusive owner of the details of a single async, unary call.
///
/// Call \ref Finish or \ref FinishWithError to send a response back to the
/// client.
///
/// If no explicit call to \p Finish or \p FinishWithError has been made
/// before this unary_call instance is destroyed or moved from, a generic
/// internal server error is sent.
///
/// @note This class can only be moved. If shared ownership semantics are
/// needed, convert this to a \ref shared_unary_call with the \ref share
/// function.
///
/// @warning A moved-from unary_call is only valid for destruction or
/// assignment.
template <typename TRequest, typename TResponse>
class unary_call final : public detail::unary_call_base<TRequest, TResponse>
{
    using base_type = detail::unary_call_base<TRequest, TResponse>;

public:
    /// @brief The default constructor of unary_call is not callable: the
    /// Bond-over-gRPC infrastructure will create unary_call instances as
    /// needed.
    ///
    /// Once created, a unary_call instance can only be moved to another
    /// unary_call or converted to a \ref shared_unary_call (see the \ref
    /// share function).
    unary_call() = delete;

    // unary_call is move-only
    unary_call(const unary_call&) = delete;
    unary_call& operator=(const unary_call&) = delete;

    /// @brief Constructs a unary_call by moving from another unary_call.
    ///
    /// The moved-from instance is valid only for destruction and
    /// assignment.
    unary_call(unary_call&& other) = default;

    /// @brief Assign to this unary_call by moving from another unary_call.
    ///
    /// The moved-from instance is valid only for destruction and
    /// assignment.
    unary_call& operator=(unary_call&& rhs) = default;

    using base_type::swap;

    /// @brief Creates a \ref shared_unary_call from this instance.
    ///
    /// This instance will be in a moved-from state after \ref share has
    /// been called, and the returned shared instance should be used to
    /// finish the call.
    shared_unary_call<TRequest, TResponse> share() && noexcept
    {
        return shared_unary_call<TRequest, TResponse>{ std::move(*this) };
    }

private:
    template <typename OtherRequest, typename OtherResponse, typename OtherThreadPool>
    friend class detail::service_unary_call_data;

    using base_type::base_type;
};

template <typename TRequest, typename TResponse>
inline void swap(unary_call<TRequest, TResponse>& lhs, unary_call<TRequest, TResponse>& rhs) noexcept
{
    lhs.swap(rhs);
}

} } } //namespace bond::ext::gRPC
