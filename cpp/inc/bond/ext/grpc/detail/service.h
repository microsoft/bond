// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/ext/grpc/scheduler.h>

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

#include <initializer_list>
#include <functional>

namespace bond { namespace ext { namespace gRPC {

class server_builder;

namespace detail {

struct io_manager_tag;

template <typename Request, typename Response>
class service_unary_call_data;

/// @brief Base class that all Bond gRPC++ services implement.
///
/// @note This class is for use by generated and helper code only.
///
/// Helper class that codegen uses to generate abstract service classes,
/// which a bond::ext::gRPC::server then hosts multiple services.
class service : private grpc::Service
{
public:
    service(const service& other) = delete;
    service& operator=(const service& other) = delete;

    /// @brief Starts the service.
    ///
    /// @note This method is for use by generated and helper code only.
    ///
    /// Typical implementations call queue_receive on all the methods in the
    /// service to kick of the process of receiving messages.
    virtual void start() = 0;

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
    /// @param tag the io_manager_tag to include with the completion queue
    /// notification
    template <typename Request>
    void queue_receive(
        int methodIndex,
        grpc::ServerContext* context,
        Request* request,
        grpc::internal::ServerAsyncStreamingInterface* responseStream,
        io_manager_tag* tag)
    {
        BOOST_ASSERT(_cq);

        RequestAsyncUnary(
            methodIndex,
            context,
            request,
            responseStream,
            _cq,
            _cq,
            tag);
    }

    /// @brief Provides access to the raw grpc::Service type.
    ///
    /// @note This method is for use by generated and helper code only.
    grpc::Service* grpc_service()
    {
        return this;
    }

    Scheduler& scheduler()
    {
        return _scheduler;
    }

protected:
    template <typename MethodT>
    using Method = service_unary_call_data<
        typename MethodT::input_type,
        typename remove_bonded<
            typename std::conditional<
                std::is_void<typename MethodT::result_type>::value,
                Void,
                typename MethodT::result_type>::type>::type>;

    service(const Scheduler& scheduler, std::initializer_list<const char*> methodNames)
        : _scheduler(scheduler ? scheduler : thread_pool{}),
          _cq(nullptr)
    {
        AddMethods(methodNames);
    }

private:
    friend class gRPC::server_builder;

    void AddMethods(std::initializer_list<const char*> names)
    {
        for (const char* name : names)
        {
            BOOST_ASSERT(name);

            // ownership of the service method is transfered to grpc::Service
            grpc::Service::AddMethod(
                new grpc::internal::RpcServiceMethod(
                    name,
                    grpc::internal::RpcMethod::NORMAL_RPC,
                    nullptr)); // nullptr indicates async handler
        }
    }

    void SetCompletionQueue(grpc::ServerCompletionQueue* cq)
    {
        BOOST_ASSERT(!_cq);
        _cq = cq;
    }

    Scheduler _scheduler;
    grpc::ServerCompletionQueue* _cq;
};

} } } } // namespace bond::ext::gRPC::detail
