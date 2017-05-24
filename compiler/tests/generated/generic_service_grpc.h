
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"

#include <bond/core/bonded.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/detail/client_call_data.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/detail/service_call_data.h>

#include <boost/optional/optional.hpp>
#include <functional>
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100 4267)
#endif

#include <grpc++/impl/codegen/channel_interface.h>
#include <grpc++/impl/codegen/client_context.h>
#include <grpc++/impl/codegen/completion_queue.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/status.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace tests
{

class Foo final
{
public:
    template <typename TThreadPool>
    class ClientCore
    {
    public:
        ClientCore(
            const std::shared_ptr< ::grpc::ChannelInterface>& channel,
            std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
            std::shared_ptr<TThreadPool> threadPool);

        void Asyncfoo31(::grpc::ClientContext* context, const ::bond::bonded<Payload>& request, std::function<void(const ::bond::bonded<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo32(::grpc::ClientContext* context, const ::bond::bonded<void>& request, std::function<void(const ::bond::bonded<Payload>&, const ::grpc::Status&)> cb);

        void Asyncfoo33(::grpc::ClientContext* context, const ::bond::bonded<Payload>& request, std::function<void(const ::bond::bonded<Payload>&, const ::grpc::Status&)> cb);

        ClientCore(const ClientCore&) = delete;
        ClientCore& operator=(const ClientCore&) = delete;

        ClientCore(ClientCore&&) = default;
        ClientCore& operator=(ClientCore&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> _channel;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        std::shared_ptr<TThreadPool> _threadPool;

        const ::grpc::RpcMethod rpcmethod_foo31_;

        const ::grpc::RpcMethod rpcmethod_foo32_;

        const ::grpc::RpcMethod rpcmethod_foo33_;
    };

    using Client = ClientCore< ::bond::ext::gRPC::thread_pool>;

    template <typename TThreadPool>
    class ServiceCore : public ::bond::ext::gRPC::detail::service<TThreadPool>
    {
    public:
        ServiceCore()
        {
            this->AddMethod("/tests.Foo/foo31");
            this->AddMethod("/tests.Foo/foo32");
            this->AddMethod("/tests.Foo/foo33");
        }

        virtual ~ServiceCore() { }
        virtual void start(
            ::grpc::ServerCompletionQueue* cq,
            std::shared_ptr<TThreadPool> tp) override
        {
            BOOST_ASSERT(cq);
            BOOST_ASSERT(tp);

            _rd_foo31.emplace(
                this,
                0,
                cq,
                tp,
                std::bind(&ServiceCore::foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(
                this,
                1,
                cq,
                tp,
                std::bind(&ServiceCore::foo32, this, std::placeholders::_1));
            _rd_foo33.emplace(
                this,
                2,
                cq,
                tp,
                std::bind(&ServiceCore::foo33, this, std::placeholders::_1));

            this->queue_receive(
                0,
                &_rd_foo31->_receivedCall->_context,
                &_rd_foo31->_receivedCall->_request,
                &_rd_foo31->_receivedCall->_responder,
                cq,
                &_rd_foo31.get());
            this->queue_receive(
                1,
                &_rd_foo32->_receivedCall->_context,
                &_rd_foo32->_receivedCall->_request,
                &_rd_foo32->_receivedCall->_responder,
                cq,
                &_rd_foo32.get());
            this->queue_receive(
                2,
                &_rd_foo33->_receivedCall->_context,
                &_rd_foo33->_receivedCall->_request,
                &_rd_foo33->_receivedCall->_responder,
                cq,
                &_rd_foo33.get());
        }

        virtual void foo31(::bond::ext::gRPC::unary_call< ::bond::bonded<Payload>, ::bond::bonded<void>>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call< ::bond::bonded<void>, ::bond::bonded<Payload>>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call< ::bond::bonded<Payload>, ::bond::bonded<Payload>>) = 0;

    private:
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<Payload>, ::bond::bonded<void>, TThreadPool>> _rd_foo31;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<void>, ::bond::bonded<Payload>, TThreadPool>> _rd_foo32;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<Payload>, ::bond::bonded<Payload>, TThreadPool>> _rd_foo33;
    };

    using Service = ServiceCore< ::bond::ext::gRPC::thread_pool>;
};

template <typename TThreadPool>
inline Foo::ClientCore<TThreadPool>::ClientCore(
    const std::shared_ptr< ::grpc::ChannelInterface>& channel,
    std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
    std::shared_ptr<TThreadPool> threadPool)
    : _channel(channel)
    , _ioManager(ioManager)
    , _threadPool(threadPool)
    , rpcmethod_foo31_("/tests.Foo/foo31", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_("/tests.Foo/foo32", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_("/tests.Foo/foo33", ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo31(
    ::grpc::ClientContext* context,
    const ::bond::bonded<Payload>& request,
    std::function<void(const ::bond::bonded<void>&, const ::grpc::Status&)> cb)
{
    auto calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::bonded<Payload>, ::bond::bonded<void>, TThreadPool >(
        _channel,
        _ioManager,
        _threadPool,
        cb);
    calldata->dispatch(rpcmethod_foo31_, context, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo32(
    ::grpc::ClientContext* context,
    const ::bond::bonded<void>& request,
    std::function<void(const ::bond::bonded<Payload>&, const ::grpc::Status&)> cb)
{
    auto calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::bonded<void>, ::bond::bonded<Payload>, TThreadPool >(
        _channel,
        _ioManager,
        _threadPool,
        cb);
    calldata->dispatch(rpcmethod_foo32_, context, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo33(
    ::grpc::ClientContext* context,
    const ::bond::bonded<Payload>& request,
    std::function<void(const ::bond::bonded<Payload>&, const ::grpc::Status&)> cb)
{
    auto calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::bonded<Payload>, ::bond::bonded<Payload>, TThreadPool >(
        _channel,
        _ioManager,
        _threadPool,
        cb);
    calldata->dispatch(rpcmethod_foo33_, context, request);
}


} // namespace tests

