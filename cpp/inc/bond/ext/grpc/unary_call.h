// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/unary_call_impl.h"

#include <boost/smart_ptr/intrusive_ptr.hpp>

#include <utility>

namespace bond { namespace ext { namespace gRPC {

template <typename Request, typename Response>
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
template <typename Request, typename Response>
class unary_call final : public detail::unary_call_base<Request, Response>
{
    using base_type = typename unary_call::unary_call_base;

public:
    using base_type::base_type;

    unary_call(const unary_call&) = delete;
    unary_call& operator=(const unary_call&) = delete;

    unary_call(unary_call&& other) = default;
    unary_call& operator=(unary_call&& rhs) = default;

    using base_type::operator bool;
    using base_type::swap;

    /// @brief Creates a \ref shared_unary_call from this instance.
    ///
    /// This instance will be in a moved-from state after \ref share has
    /// been called, and the returned shared instance should be used to
    /// finish the call.
    shared_unary_call<Request, Response> share() && noexcept
    {
        return shared_unary_call<Request, Response>{ std::move(*this) };
    }
};

template <typename Request, typename Response>
inline void swap(unary_call<Request, Response>& lhs, unary_call<Request, Response>& rhs) noexcept
{
    lhs.swap(rhs);
}

} } } //namespace bond::ext::gRPC
