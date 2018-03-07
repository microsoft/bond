
#pragma once

#include "service_attributes_reflection.h"
#include "service_attributes_types.h"


#include <bond/core/bonded.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/client_callback.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/reflection.h>
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

#include <grpcpp/impl/codegen/channel_interface.h>
#include <grpcpp/impl/codegen/client_context.h>
#include <grpcpp/impl/codegen/completion_queue.h>
#include <grpcpp/impl/codegen/rpc_method.h>
#include <grpcpp/impl/codegen/status.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace tests
{

class Foo final
{
public:
    struct Schema;

    template <typename TThreadPool>
    class ClientCore
    {
    public:
        ClientCore(
            const std::shared_ptr< ::grpc::ChannelInterface>& channel,
            std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
            std::shared_ptr<TThreadPool> threadPool);

        void Asyncfoo(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::Param>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::Result>>)>& cb);
        void Asyncfoo(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::Param& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::Result>>)>& cb)
        {
            Asyncfoo(context, ::bond::bonded< ::tests::Param>{request}, cb);
        }
        void Asyncfoo(const ::bond::bonded< ::tests::Param>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::Result>>)>& cb)
        {
            Asyncfoo(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo(const ::tests::Param& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::Result>>)>& cb)
        {
            Asyncfoo(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::Param>{request}, cb);
        }

        ClientCore(const ClientCore&) = delete;
        ClientCore& operator=(const ClientCore&) = delete;

        ClientCore(ClientCore&&) = default;
        ClientCore& operator=(ClientCore&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> _channel;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        std::shared_ptr<TThreadPool> _threadPool;

        const ::grpc::internal::RpcMethod rpcmethod_foo_;
    };

    using Client = ClientCore< ::bond::ext::gRPC::thread_pool>;

    template <typename TThreadPool>
    class ServiceCore : public ::bond::ext::gRPC::detail::service<TThreadPool>
    {
    public:
        ServiceCore()
        {
            this->AddMethod("/tests.Foo/foo");
        }

        virtual ~ServiceCore() { }
        virtual void start(
            ::grpc::ServerCompletionQueue* cq,
            std::shared_ptr<TThreadPool> tp) override
        {
            BOOST_ASSERT(cq);
            BOOST_ASSERT(tp);

            _rd_foo.emplace(
                this,
                0,
                cq,
                tp,
                std::bind(&ServiceCore::foo, this, std::placeholders::_1));

            this->queue_receive(
                0,
                &_rd_foo->_receivedCall->context(),
                &_rd_foo->_receivedCall->request(),
                &_rd_foo->_receivedCall->responder(),
                cq,
                &_rd_foo.get());
        }

        virtual void foo(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::Param>, ::tests::Result>) = 0;

    private:
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::Param>, ::tests::Result, TThreadPool>> _rd_foo;
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
    , rpcmethod_foo_("/tests.Foo/foo", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
    { }

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::Param>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::Result>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::Param, ::tests::Result, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo_, request);
}

struct Foo::Schema
{
    static const ::bond::Metadata metadata;

    private: static const ::bond::Metadata s_foo_metadata;

    public: struct service
    {
        typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::Param>,
                ::bond::bonded< ::tests::Result>,
                &s_foo_metadata
            > foo;
    };

    private: typedef boost::mpl::list<> methods0;
    private: typedef boost::mpl::push_front<methods0, service::foo>::type methods1;

    public: typedef methods1::type methods;

    
};



} // namespace tests

