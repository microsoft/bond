// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/rpc_method.h>
#include <grpcpp/impl/codegen/rpc_service_method.h>
#include <grpcpp/impl/codegen/service_type.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>

namespace bond { namespace ext { namespace gRPC { namespace detail {

struct io_manager_tag;

/// @brief Base class that all Bond gRPC++ services implement.
///
/// @note This class is for use by generated and helper code only.
///
/// Helper class that codegen uses to generate abstract service classes,
/// which a bond::ext::gRPC::server then hosts multiple services.
template <typename TThreadPool>
class service : private grpc::Service
{
public:
    virtual ~service() { }

    /// @brief Starts the service.
    ///
    /// @note This method is for use by generated and helper code only.
    ///
    /// Typical implementations call queue_receive on all the methods in the
    /// service to kick of the process of receiving messages.
    virtual void start(grpc::ServerCompletionQueue* cq, std::shared_ptr<TThreadPool> threadPool) = 0;

    /// @brief Starts the receive process for a method.
    ///
    /// @note This method is for use by generated and helper code only.
    ///
    /// When a request for the method has been received, \p tag will be
    /// added to \p cq.
    ///
    /// @param methodIndex the index of the method (indices are assigned by
    /// the order in which the methods are registered via calls to
    /// AddMethod)
    ///
    /// @param context a fresh grpc::ServerContext for the call to populate
    ///
    /// @param request pointer to a request object to populate
    ///
    /// @param responseStream pointer to a response stream to populate
    ///
    /// @param cq the completion queue to notify when a call has been
    /// received
    ///
    /// @param tag the io_manager_tag to include with the completion queue
    /// notification
    template <typename TRequest>
    void queue_receive(
        int methodIndex,
        grpc::ServerContext* context,
        TRequest* request,
        grpc::internal::ServerAsyncStreamingInterface* responseStream,
        grpc::ServerCompletionQueue* cq,
        io_manager_tag* tag)
    {
        RequestAsyncUnary(
            methodIndex,
            context,
            request,
            responseStream,
            cq,
            cq,
            tag);
    }

    /// @brief Provides access to the raw grpc::Service type.
    ///
    /// @note This method is for use by generated and helper code only.
    grpc::Service* grpc_service()
    {
        return this;
    }

protected:
    /// @brief Registers a method name for dispatch to this service.
    ///
    /// @note This method is for use by generated and helper code only.
    ///
    /// The order in which methods are registered assigned the method index,
    /// which is used elsewhere.
    void AddMethod(const char* methodName)
    {
        BOOST_ASSERT(methodName);

        // ownership of the service method is transfered to grpc::Service
        AddMethod(
            new grpc::internal::RpcServiceMethod(
                methodName,
                grpc::internal::RpcMethod::NORMAL_RPC,
                nullptr)); // nullptr indicates async handler
    }

private:
    using grpc::Service::AddMethod;
};

} } } } //namespace bond::ext::gRPC::detail
