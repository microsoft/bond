
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"

#include <bond/core/bond_reflection.h>
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

template <typename Payload>
    struct Foo final
{
    struct Schema
    {
        static const ::bond::Metadata metadata;

        private: static const ::bond::Metadata s_foo31_metadata;
        private: static const ::bond::Metadata s_foo32_metadata;
        private: static const ::bond::Metadata s_foo33_metadata;
        private: static const ::bond::Metadata s_ConsumesGeneric1_metadata;
        private: static const ::bond::Metadata s_ConsumesGeneric2_metadata;

        public: struct service
        {
            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded<Payload>,
                ::bond::bonded< ::bond::Void>,
                &s_foo31_metadata
            > foo31;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded<Payload>,
                &s_foo32_metadata
            > foo32;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded<Payload>,
                ::bond::bonded<Payload>,
                &s_foo33_metadata
            > foo33;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::SomeBox<int32_t>>,
                ::bond::bonded< ::bond::Void>,
                &s_ConsumesGeneric1_metadata
            > ConsumesGeneric1;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>,
                ::bond::bonded< ::bond::Void>,
                &s_ConsumesGeneric2_metadata
            > ConsumesGeneric2;
        };

        private: typedef boost::mpl::list<> methods0;
        private: typedef typename boost::mpl::push_front<methods0, typename service::ConsumesGeneric2>::type methods1;
        private: typedef typename boost::mpl::push_front<methods1, typename service::ConsumesGeneric1>::type methods2;
        private: typedef typename boost::mpl::push_front<methods2, typename service::foo33>::type methods3;
        private: typedef typename boost::mpl::push_front<methods3, typename service::foo32>::type methods4;
        private: typedef typename boost::mpl::push_front<methods4, typename service::foo31>::type methods5;

        public: typedef typename methods5::type methods;

        Schema()
        {
            // Force instantiation of template statics
            (void)metadata;
            (void)s_foo31_metadata;
            (void)s_foo32_metadata;
            (void)s_foo33_metadata;
            (void)s_ConsumesGeneric1_metadata;
            (void)s_ConsumesGeneric2_metadata;
        }
    };

    class Client
    {
    public:
        Client(
            const std::shared_ptr< ::grpc::ChannelInterface>& channel,
            std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
            const ::bond::ext::gRPC::Scheduler& scheduler = {})
            : _channel(channel)
            , _ioManager(ioManager)
            , _scheduler(scheduler)
            , rpcmethod_foo31_("/tests.Foo/foo31", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo32_("/tests.Foo/foo32", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo33_("/tests.Foo/foo33", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_ConsumesGeneric1_("/tests.Foo/ConsumesGeneric1", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_ConsumesGeneric2_("/tests.Foo/ConsumesGeneric2", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
        {
            if (!_scheduler)
            {
                _scheduler = ::bond::ext::gRPC::thread_pool{};
            }
        }

        void Asyncfoo31(const ::bond::bonded<Payload>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< Payload, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo31_, request);
        }
        void Asyncfoo31(const Payload& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo31(::bond::bonded<Payload>{request}, cb, ::std::move(context));
        }

        void Asyncfoo32(const ::std::function<void(::bond::ext::gRPC::unary_call_result< Payload>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, Payload>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo32_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo33(const ::bond::bonded<Payload>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< Payload>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< Payload, Payload>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo33_, request);
        }
        void Asyncfoo33(const Payload& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< Payload>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo33(::bond::bonded<Payload>{request}, cb, ::std::move(context));
        }

        void AsyncConsumesGeneric1(const ::bond::bonded< ::tests::SomeBox<int32_t>>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::SomeBox<int32_t>, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_ConsumesGeneric1_, request);
        }
        void AsyncConsumesGeneric1(const ::tests::SomeBox<int32_t>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            AsyncConsumesGeneric1(::bond::bonded< ::tests::SomeBox<int32_t>>{request}, cb, ::std::move(context));
        }

        void AsyncConsumesGeneric2(const ::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::SomeBox<std::vector<int32_t> >, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_ConsumesGeneric2_, request);
        }
        void AsyncConsumesGeneric2(const ::tests::SomeBox<std::vector<int32_t> >& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            AsyncConsumesGeneric2(::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>{request}, cb, ::std::move(context));
        }

        Client(const Client&) = delete;
        Client& operator=(const Client&) = delete;

        Client(Client&&) = default;
        Client& operator=(Client&&) = default;

    private:
        ::std::shared_ptr< ::grpc::ChannelInterface> _channel;
        ::std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        ::bond::ext::gRPC::Scheduler _scheduler;

        const ::grpc::internal::RpcMethod rpcmethod_foo31_;
        const ::grpc::internal::RpcMethod rpcmethod_foo32_;
        const ::grpc::internal::RpcMethod rpcmethod_foo33_;
        const ::grpc::internal::RpcMethod rpcmethod_ConsumesGeneric1_;
        const ::grpc::internal::RpcMethod rpcmethod_ConsumesGeneric2_;
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            this->AddMethod("/tests.Foo/foo31");
            this->AddMethod("/tests.Foo/foo32");
            this->AddMethod("/tests.Foo/foo33");
            this->AddMethod("/tests.Foo/ConsumesGeneric1");
            this->AddMethod("/tests.Foo/ConsumesGeneric2");
        }

        virtual void start(
            ::grpc::ServerCompletionQueue* cq,
            const ::bond::ext::gRPC::Scheduler& scheduler) override
        {
            BOOST_ASSERT(cq);
            BOOST_ASSERT(scheduler);

            _rd_foo31.emplace(
                *this,
                0,
                cq,
                scheduler,
                std::bind(&Service::foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(
                *this,
                1,
                cq,
                scheduler,
                std::bind(&Service::foo32, this, std::placeholders::_1));
            _rd_foo33.emplace(
                *this,
                2,
                cq,
                scheduler,
                std::bind(&Service::foo33, this, std::placeholders::_1));
            _rd_ConsumesGeneric1.emplace(
                *this,
                3,
                cq,
                scheduler,
                std::bind(&Service::ConsumesGeneric1, this, std::placeholders::_1));
            _rd_ConsumesGeneric2.emplace(
                *this,
                4,
                cq,
                scheduler,
                std::bind(&Service::ConsumesGeneric2, this, std::placeholders::_1));
        }

        virtual void foo31(::bond::ext::gRPC::unary_call< ::bond::bonded<Payload>, ::bond::Void>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, Payload>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call< ::bond::bonded<Payload>, Payload>) = 0;
        virtual void ConsumesGeneric1(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::SomeBox<int32_t>>, ::bond::Void>) = 0;
        virtual void ConsumesGeneric2(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>, ::bond::Void>) = 0;

    private:
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<Payload>, ::bond::Void>> _rd_foo31;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, Payload>> _rd_foo32;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded<Payload>, Payload>> _rd_foo33;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::SomeBox<int32_t>>, ::bond::Void>> _rd_ConsumesGeneric1;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>, ::bond::Void>> _rd_ConsumesGeneric2;
    };
};


    template <typename Payload>
    const ::bond::Metadata Foo<Payload>::Schema::metadata
        = ::bond::reflection::MetadataInit<boost::mpl::list<Payload> >("Foo", "tests.Foo",
                ::bond::reflection::Attributes());
    
    template <typename Payload>
    const ::bond::Metadata Foo<Payload>::Schema::s_foo31_metadata
        = ::bond::reflection::MetadataInit("foo31");
    
    template <typename Payload>
    const ::bond::Metadata Foo<Payload>::Schema::s_foo32_metadata
        = ::bond::reflection::MetadataInit("foo32");
    
    template <typename Payload>
    const ::bond::Metadata Foo<Payload>::Schema::s_foo33_metadata
        = ::bond::reflection::MetadataInit("foo33");
    
    template <typename Payload>
    const ::bond::Metadata Foo<Payload>::Schema::s_ConsumesGeneric1_metadata
        = ::bond::reflection::MetadataInit("ConsumesGeneric1");
    
    template <typename Payload>
    const ::bond::Metadata Foo<Payload>::Schema::s_ConsumesGeneric2_metadata
        = ::bond::reflection::MetadataInit("ConsumesGeneric2");


} // namespace tests

