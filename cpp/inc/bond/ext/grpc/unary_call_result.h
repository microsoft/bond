// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/lazy_bonded.h"

#include <bond/core/bonded.h>

#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/client_context.h>

#include <memory>

namespace bond { namespace ext { namespace grpc
{
    /// @brief The client-side results of a unary call.
    template <typename Response>
    class unary_call_result;

    template <>
    class unary_call_result<void>
    {
    public:
#ifdef _MSC_VER
        unary_call_result() = default; // Workaround for a buggy std::promise in MSVC.
#endif
        /// @brief Create a unary_call_result with the given values.
        ///
        /// @param status The status.
        /// @param context the context under which the request is being executed.
        unary_call_result(
            const ::grpc::ByteBuffer& /*responseBuffer*/,
            const ::grpc::Status& status,
            std::shared_ptr<::grpc::ClientContext> context)
            : _status(status),
              _context(std::move(context))
        {}

        /// @brief The status of the request.
        const ::grpc::Status& status() const noexcept
        {
            return _status;
        }

        /// @brief The client context under which the request was executed.
        const std::shared_ptr<::grpc::ClientContext>& context() const noexcept
        {
            return _context;
        }

    private:
        ::grpc::Status _status;
        std::shared_ptr<::grpc::ClientContext> _context;
    };


    /// @brief The client-side results of a unary call.
    template <typename Response>
    class unary_call_result : public unary_call_result<void>
    {
    public:
#ifdef _MSC_VER
        unary_call_result() = default; // Workaround for a buggy std::promise in MSVC.
#endif
        /// @brief Create a unary_call_result with the given values.
        ///
        /// @param response The response.
        /// @param status The status.
        /// @param context the context under which the request is being executed.
        unary_call_result(
            const ::grpc::ByteBuffer& responseBuffer,
            const ::grpc::Status& status,
            std::shared_ptr<::grpc::ClientContext> context)
            : unary_call_result<void>(responseBuffer, status, std::move(context)),
              _response{ responseBuffer }
        {}

        /// @brief The response received from the service.
        ///
        /// @note Depending on the implementation of the service, this may or
        /// may not contain an actual response. Consult the documentation for
        /// the service to determine under what conditions it sends back a
        /// response.
        const bonded<Response>& response() const
        {
            return _response.get();
        }

    private:
        detail::lazy_bonded<Response> _response;
    };

} } } // namespace bond::ext::grpc
