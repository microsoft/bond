// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/exception.h>

#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/client_context.h>

namespace bond { namespace ext { namespace gRPC {

    /// @brief %Exception thrown to indicate that a callback has been
    /// invoked multiple times when only one invocation is expected.
    class MultipleInvocationException : public Exception
    {
    public:
        MultipleInvocationException() : Exception("The callback was invoked more than once.") { }
    };

    class UnaryCallException : public Exception
    {
    public:
        UnaryCallException(const grpc::Status& status, std::shared_ptr<grpc::ClientContext> context)
            : Exception{ status.error_message().c_str() },
              _status{ status },
              _context{ std::move(context) }
        {}

        const grpc::Status& status() const BOND_NOEXCEPT
        {
            return _status;
        }

        const std::shared_ptr<grpc::ClientContext>& context() const BOND_NOEXCEPT
        {
            return _context;
        }

    private:
        const grpc::Status _status;
        const std::shared_ptr<grpc::ClientContext> _context;
    };

} } } // namespace bond::ext::gRPC
