
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

struct Foo final
{
    struct Schema
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

    class Client
    {
    public:
        Client(
            const std::shared_ptr< ::grpc::ChannelInterface>& channel,
            std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
            const ::bond::ext::gRPC::Scheduler& scheduler)
            : _channel(channel)
            , _ioManager(ioManager)
            , _scheduler(scheduler)
            , rpcmethod_foo_("/tests.Foo/foo", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
        {
            BOOST_ASSERT(_scheduler);
        }

        void Asyncfoo(const ::bond::bonded< ::tests::Param>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::Result>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::Param, ::tests::Result>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo_, request);
        }
        void Asyncfoo(const ::tests::Param& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::Result>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo(::bond::bonded< ::tests::Param>{request}, cb, ::std::move(context));
        }

        Client(const Client&) = delete;
        Client& operator=(const Client&) = delete;

        Client(Client&&) = default;
        Client& operator=(Client&&) = default;

    private:
        ::std::shared_ptr< ::grpc::ChannelInterface> _channel;
        ::std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        ::bond::ext::gRPC::Scheduler _scheduler;

        const ::grpc::internal::RpcMethod rpcmethod_foo_;
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            this->AddMethod("/tests.Foo/foo");
        }

        virtual void start(
            ::grpc::ServerCompletionQueue* cq,
            const ::bond::ext::gRPC::Scheduler& scheduler) override
        {
            BOOST_ASSERT(cq);
            BOOST_ASSERT(scheduler);

            _rd_foo.emplace(
                *this,
                0,
                cq,
                scheduler,
                std::bind(&Service::foo, this, std::placeholders::_1));
        }

        virtual void foo(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::Param>, ::tests::Result>) = 0;

    private:
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::Param>, ::tests::Result>> _rd_foo;
    };
};




} // namespace tests

