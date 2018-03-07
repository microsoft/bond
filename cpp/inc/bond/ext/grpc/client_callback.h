// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bonded.h>

#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/client_context.h>

#include <memory>
#include <utility>

namespace bond { namespace ext { namespace gRPC {

    /// @brief The client-side results of a unary call.
    template <typename TResponse>
    struct unary_call_result
    {
        /// @brief The response received from the service.
        ///
        /// @note Depending on the implementation of the service, this may or
        /// may not contain an actual response. Consult the documentation for
        /// the service to determine under what conditions it sends back a
        /// response.
        bond::bonded<TResponse> response;
        /// @brief The status of the request.
        grpc::Status status;
        /// @brief The client context underwhich the request was executed.
        std::shared_ptr<grpc::ClientContext> context;

        /// @brief Create a unary_call_result with the given context and
        /// empty \ref response and \ref status members.
        ///
        /// @param context the context underwhich the request is being
        /// executed.
        explicit unary_call_result(std::shared_ptr<grpc::ClientContext> context)
            : response(),
              status(),
              context(std::move(context))
        { }

        /// @brief Create a unary_call_result with the given values.
        ///
        /// @param response The response.
        /// @param status The status.
        /// @param context the context underwhich the request is being executed.
        unary_call_result(
            const bond::bonded<TResponse>& response,
            const grpc::Status& status,
            std::shared_ptr<grpc::ClientContext> context)
            : response(response),
              status(status),
              context(std::move(context))
        { }

        unary_call_result(const unary_call_result&) = default;
        unary_call_result(unary_call_result&&) = default;
        unary_call_result& operator=(const unary_call_result&) = default;
        unary_call_result& operator=(unary_call_result&&) = default;
    };

} } } // namespace bond::ext::gRPC
