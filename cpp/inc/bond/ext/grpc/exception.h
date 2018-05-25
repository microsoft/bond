// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/exception.h>

#include <grpcpp/client_context.h>
#include <grpcpp/impl/codegen/status.h>

namespace bond { namespace ext { namespace grpc
{
    /// @brief %Exception thrown to indicate that a callback has been
    /// invoked multiple times when only one invocation is expected.
    class MultipleInvocationException : public Exception
    {
    public:
        MultipleInvocationException()
            : Exception{ "The callback was invoked more than once." }
        {}
    };

    /// @brief %Exception thrown when std::thread::hardware_concurrency
    /// returns 0.
    class InvalidThreadCount : public Exception
    {
    public:
        InvalidThreadCount()
            : Exception{ "Invalid number of threads." }
        {}
    };

    /// @brief %Exception thrown when gRPC APIs return failure.
    class GrpcException : public Exception
    {
    public:
        explicit GrpcException(const ::grpc::Status& status)
            : Exception{ status.error_message().c_str() },
              _status{ status }
        {}

        const ::grpc::Status& status() const noexcept
        {
            return _status;
        }

    private:
        const ::grpc::Status _status;
    };

    /// @brief %Exception thrown when proxy invocation returns failure status.
    class UnaryCallException : public GrpcException
    {
    public:
        UnaryCallException(const ::grpc::Status& status, std::shared_ptr<::grpc::ClientContext> context)
            : GrpcException{ status },
              _context{ std::move(context) }
        {}

        const std::shared_ptr<::grpc::ClientContext>& context() const noexcept
        {
            return _context;
        }

    private:
        const std::shared_ptr<::grpc::ClientContext> _context;
    };

    /// @brief %Exception thrown when unable to construct a ::grpc::Server.
    class ServerBuildException : public Exception
    {
    public:
        ServerBuildException()
            : Exception{ "Failed to build a ::grpc::Server." }
        {}
    };

} } } // namespace bond::ext::grpc
