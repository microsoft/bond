// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "io_manager_tag.h"
#include "serialization.h"

#include <bond/core/bonded.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/scheduler.h>
#include <bond/ext/grpc/unary_call_result.h>

#ifdef _MSC_VER
    #pragma warning (push)
    // warning C4100: unreferenced formal parameter
    //
    // warning C4127: conditional expression is constant
    //
    // warning C4702: unreachable code
    //
    // warning C4800: 'int': forcing value to bool 'true' or 'false' (performance warning)
    #pragma warning (disable: 4100 4127 4702 4800)
#endif

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/async_unary_call.h>
#include <grpcpp/impl/codegen/channel_interface.h>
#include <grpcpp/impl/codegen/client_context.h>
#include <grpcpp/impl/codegen/completion_queue.h>
#include <grpcpp/impl/codegen/rpc_method.h>
#include <grpcpp/impl/codegen/status.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/intrusive_ptr.hpp>
#include <boost/smart_ptr/intrusive_ref_counter.hpp>

#include <functional>
#include <future>
#include <memory>

namespace bond { namespace ext { namespace grpc { namespace detail
{
    /// @brief Helper base class Bond grpc++ clients.
    ///
    /// @note This class is for use by generated and helper code only.
    class client
    {
    public:
        client(
            std::shared_ptr<::grpc::ChannelInterface> channel,
            std::shared_ptr<io_manager> ioManager,
            const Scheduler& scheduler)
            : _channel{ std::move(channel) },
              _ioManager{ std::move(ioManager) },
              _scheduler{ scheduler }
        {
            BOOST_ASSERT(_scheduler);
        }

        client(const client& other) = delete;
        client& operator=(const client& other) = delete;

        client(client&& other) = default;
        client& operator=(client&& other) = default;

    protected:
#if !defined(__GNUC__) || (__GNUC__ > 7) || (__GNUC__ == 7 && __GNUC_MINOR__ >= 2)
        using Method = ::grpc::internal::RpcMethod;
#else
        // Workaround for a bug in GCC < 7.2: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=67054.
        struct Method : ::grpc::internal::RpcMethod
        {
            using ::grpc::internal::RpcMethod::RpcMethod;
            Method();
        };
#endif
        Method make_method(const char* name) const
        {
            return Method{ name, ::grpc::internal::RpcMethod::NORMAL_RPC, _channel };
        }

        template <typename Response = void, typename Request = Void>
        void dispatch(
            const ::grpc::internal::RpcMethod& method,
            std::shared_ptr<::grpc::ClientContext> context,
            const std::function<void(unary_call_result<Response>)>& cb,
            const bonded<Request>& request);

        template <typename Response = void, typename Request = Void>
        void dispatch(
            const ::grpc::internal::RpcMethod& method,
            std::shared_ptr<::grpc::ClientContext> context,
            const std::function<void(unary_call_result<Response>)>& cb,
            const Request& request = {})
        {
            dispatch(method, std::move(context), cb, bonded<Request>{ boost::ref(request) });
        }

        template <typename Response, typename Request = Void>
        std::future<unary_call_result<Response>> dispatch(
            const ::grpc::internal::RpcMethod& method,
            std::shared_ptr<::grpc::ClientContext> context,
            const bonded<Request>& request)
        {
            auto callback = std::make_shared<std::packaged_task<unary_call_result<Response>(unary_call_result<Response>)>>(
                [](unary_call_result<Response> response)
                {
                    return response.status().ok()
                        ? response
                        : throw UnaryCallException{ response.status(), response.context() };
                });

            auto result = callback->get_future();

            std::function<void(unary_call_result<Response>)> copyableCallback =
                [callback](unary_call_result<Response> response)
                {
                    (*callback)(std::move(response));
                };

            dispatch(method, std::move(context), copyableCallback, request);

            return result;
        }

        template <typename Response, typename Request = Void>
        std::future<unary_call_result<Response>> dispatch(
            const ::grpc::internal::RpcMethod& method,
            std::shared_ptr<::grpc::ClientContext> context,
            const Request& request = {})
        {
            return dispatch<Response>(method, std::move(context), bonded<Request>{ boost::ref(request) });
        }

    private:
        class unary_call_data;

        std::shared_ptr<::grpc::ChannelInterface> _channel;
        std::shared_ptr<io_manager> _ioManager;
        Scheduler _scheduler;
    };


    /// @brief Implementation class that hold the state associated with
    /// outgoing unary calls.
    class client::unary_call_data
        : public boost::intrusive_ref_counter<unary_call_data>,
          io_manager_tag
    {
    public:
        template <typename Response>
        unary_call_data(
            const ::grpc::internal::RpcMethod& method,
            const ::grpc::ByteBuffer& requestBuffer,
            std::shared_ptr<::grpc::CompletionQueue> cq,
            std::shared_ptr<::grpc::ChannelInterface> channel,
            std::shared_ptr<::grpc::ClientContext> context,
            const Scheduler& scheduler,
            const std::function<void(unary_call_result<Response>)>& cb)
            : _cq(std::move(cq)),
              _channel(std::move(channel)),
              _context(std::move(context)),
              _responseReader(
                  ::grpc::internal::ClientAsyncResponseReaderFactory<::grpc::ByteBuffer>::Create(
                      _channel.get(),
                      _cq.get(),
                      method,
                      _context.get(),
                      requestBuffer,
                      /* start */ true)),
              _scheduler(scheduler),
              _responseBuffer(),
              _status(),
              _self(this)
        {
            BOOST_ASSERT(_scheduler);

            if (cb)
            {
                _invoke = std::bind(&unary_call_data::invoke<Response>, this, cb);
            }

            auto self = _self; // Make sure `this` will outlive the below call.
            _responseReader->Finish(&_responseBuffer, &_status, tag());
        }

    private:
        template <typename Response>
        void invoke(const std::function<void(unary_call_result<Response>)>& callback)
        {
            // TODO: Use lambda with move-capture when allowed to use C++14.
            _scheduler(std::bind(
                [](decltype(callback)& cb,
                    ::grpc::ByteBuffer& responseBuffer,
                    ::grpc::Status& status,
                    std::shared_ptr<::grpc::ClientContext>& context)
                {
                    cb(unary_call_result<Response>{ std::move(responseBuffer), status, std::move(context) });
                },
                callback,
                std::move(_responseBuffer),
                std::move(_status),
                std::move(_context)));
        }

        /// @brief Invoked after the response has been received.
        void invoke(bool ok) override
        {
            if (ok && _invoke)
            {
                _invoke();
            }

            _self.reset();
        }

        /// The completion port to post IO operations to.
        std::shared_ptr<::grpc::CompletionQueue> _cq;
        /// The channel to send the request on.
        std::shared_ptr<::grpc::ChannelInterface> _channel;
        /// @brief The client context under which the request was executed.
        std::shared_ptr<::grpc::ClientContext> _context;
        /// A response reader.
        std::unique_ptr<::grpc::ClientAsyncResponseReader<::grpc::ByteBuffer>> _responseReader;
        /// The scheduler in which to invoke the callback.
        Scheduler _scheduler;
        /// @brief The response buffer received from the service.
        /*::grpc::*/ByteBuffer _responseBuffer;
        /// @brief The status of the request.
        ::grpc::Status _status;
        /// @brief Type-erased function to invoke user-callback for a response.
        std::function<void()> _invoke;
        /// A pointer to ourselves used to keep us alive while waiting to
        /// receive the response.
        boost::intrusive_ptr<unary_call_data> _self;
    };


    template <typename Response, typename Request>
    void client::dispatch(
        const ::grpc::internal::RpcMethod& method,
        std::shared_ptr<::grpc::ClientContext> context,
        const std::function<void(unary_call_result<Response>)>& cb,
        const bonded<Request>& request)
    {
        new unary_call_data{
            method,
            Serialize(request),
            _ioManager->shared_cq(),
            _channel,
            context ? std::move(context) : std::make_shared<::grpc::ClientContext>(),
            _scheduler,
            cb };
    }

} } } } // namespace bond::ext::grpc::detail
