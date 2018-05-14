// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bonded.h>

#include <grpcpp/impl/codegen/status.h>

#include <memory>


namespace bond { namespace ext { namespace gRPC {

    /// @brief The client-side results of a unary call.
    template <typename Response>
    class unary_call_result
    {
    public:
        /// @brief Create a unary_call_result with the given values.
        ///
        /// @param response The response.
        /// @param status The status.
        unary_call_result(bonded<Response> response, const grpc::Status& status)
            : _response(std::move(response)),
              _status(status)
        { }

        const bonded<Response>& response() const BOND_NOEXCEPT
        {
            return _response;
        }

        const grpc::Status& status() const BOND_NOEXCEPT
        {
            return _status;
        }

    private:
        /// @brief The response received from the service.
        ///
        /// @note Depending on the implementation of the service, this may or
        /// may not contain an actual response. Consult the documentation for
        /// the service to determine under what conditions it sends back a
        /// response.
        bonded<Response> _response;
        /// @brief The status of the request.
        grpc::Status _status;
    };

} } } // namespace bond::ext::gRPC
