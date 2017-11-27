// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/ext/grpc/detail/unary_call_impl.h>
#include <bond/ext/grpc/unary_call.h>

#include <utility>

namespace bond { namespace ext { namespace gRPC {

/// @brief A shared owner of the details of a single async, unary call.
///
/// Call \ref Finish or \ref FinishWithError to send a response back to the
/// client.
///
/// To obtain a shared_unary_call instance, call \ref unary_call::share or
/// construct a shared_unary_call from a \ref unary_call rvalue (e.g., via
/// moving).
///
/// If no explicit call to \p Finish or \p FinishWithError has been made before all
/// the shared owners are destroyed, a generic internal server error is
/// sent.
template <typename TRequest, typename TResponse>
class shared_unary_call final : public detail::unary_call_base<TRequest, TResponse>
{
    using base_type = detail::unary_call_base<TRequest, TResponse>;

public:
    /// @brief Creates an empty shared_unary_call.
    ///
    /// @warning An empty shared_unary_call can only be destroyed or
    /// assigned to.
    shared_unary_call() = default;

    /// @brief Create a shared_unary_call by moving from an existing
    /// unary_call.
    ///
    /// The shared_unary_call assumes ownership, and the provided unary_call
    /// will be in the moved-from state.
    explicit shared_unary_call(unary_call<TRequest, TResponse> other) noexcept
        : base_type(std::move(other))
    { }

    #ifdef BOND_DOXYGEN_ONLY
    /// @brief Returns true if this shared_unary_call is non-empty;
    /// otherwise false.
    ///
    /// @warning An empty shared_unary_call can only be destroyed or
    /// assigned to.
    explicit operator bool() const;
    #endif

    using base_type::operator bool;

    using base_type::swap;
};

template <typename TRequest, typename TResponse>
inline void swap(shared_unary_call<TRequest, TResponse>& lhs, shared_unary_call<TRequest, TResponse>& rhs) noexcept
{
    lhs.swap(rhs);
}

} } } //namespace bond::ext::gRPC
