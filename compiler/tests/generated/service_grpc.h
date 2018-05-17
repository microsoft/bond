
#pragma once

#include "service_reflection.h"
#include "service_types.h"
#include "basic_types_grpc.h"
#include "namespace_basic_types_grpc.h"
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

struct Foo final
{
    struct Schema
    {
        static const ::bond::Metadata metadata;

        private: static const ::bond::Metadata s_foo11_metadata;
        private: static const ::bond::Metadata s_foo12_metadata;
        private: static const ::bond::Metadata s_foo12_impl_metadata;
        private: static const ::bond::Metadata s_foo13_metadata;
        private: static const ::bond::Metadata s_foo14_metadata;
        private: static const ::bond::Metadata s_foo15_metadata;
        private: static const ::bond::Metadata s_foo21_metadata;
        private: static const ::bond::Metadata s_foo22_metadata;
        private: static const ::bond::Metadata s_foo23_metadata;
        private: static const ::bond::Metadata s_foo24_metadata;
        private: static const ::bond::Metadata s_foo31_metadata;
        private: static const ::bond::Metadata s_foo32_metadata;
        private: static const ::bond::Metadata s_foo33_metadata;
        private: static const ::bond::Metadata s__rd_foo33_metadata;
        private: static const ::bond::Metadata s_foo34_metadata;
        private: static const ::bond::Metadata s_foo41_metadata;
        private: static const ::bond::Metadata s_foo42_metadata;
        private: static const ::bond::Metadata s_foo43_metadata;
        private: static const ::bond::Metadata s_foo44_metadata;
        private: static const ::bond::Metadata s_cq_metadata;

        public: struct service
        {
            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                void,
                &s_foo11_metadata
            > foo11;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                void,
                &s_foo12_metadata
            > foo12;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                void,
                &s_foo12_impl_metadata
            > foo12_impl;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::BasicTypes>,
                void,
                &s_foo13_metadata
            > foo13;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::dummy>,
                void,
                &s_foo14_metadata
            > foo14;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests2::OtherBasicTypes>,
                void,
                &s_foo15_metadata
            > foo15;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::bond::Void>,
                &s_foo21_metadata
            > foo21;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::bond::Void>,
                &s_foo22_metadata
            > foo22;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::BasicTypes>,
                ::bond::bonded< ::bond::Void>,
                &s_foo23_metadata
            > foo23;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::dummy>,
                ::bond::bonded< ::bond::Void>,
                &s_foo24_metadata
            > foo24;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::tests::BasicTypes>,
                &s_foo31_metadata
            > foo31;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::tests::BasicTypes>,
                &s_foo32_metadata
            > foo32;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::BasicTypes>,
                ::bond::bonded< ::tests::BasicTypes>,
                &s_foo33_metadata
            > foo33;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::BasicTypes>,
                ::bond::bonded< ::tests::BasicTypes>,
                &s__rd_foo33_metadata
            > _rd_foo33;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::dummy>,
                ::bond::bonded< ::tests::BasicTypes>,
                &s_foo34_metadata
            > foo34;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::tests::dummy>,
                &s_foo41_metadata
            > foo41;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::tests::dummy>,
                &s_foo42_metadata
            > foo42;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::BasicTypes>,
                ::bond::bonded< ::tests::dummy>,
                &s_foo43_metadata
            > foo43;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::tests::dummy>,
                ::bond::bonded< ::tests::dummy>,
                &s_foo44_metadata
            > foo44;

            typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                Foo,
                ::bond::bonded< ::bond::Void>,
                ::bond::bonded< ::tests::BasicTypes>,
                &s_cq_metadata
            > cq;
        };

        private: typedef boost::mpl::list<> methods0;
        private: typedef boost::mpl::push_front<methods0, service::cq>::type methods1;
        private: typedef boost::mpl::push_front<methods1, service::foo44>::type methods2;
        private: typedef boost::mpl::push_front<methods2, service::foo43>::type methods3;
        private: typedef boost::mpl::push_front<methods3, service::foo42>::type methods4;
        private: typedef boost::mpl::push_front<methods4, service::foo41>::type methods5;
        private: typedef boost::mpl::push_front<methods5, service::foo34>::type methods6;
        private: typedef boost::mpl::push_front<methods6, service::_rd_foo33>::type methods7;
        private: typedef boost::mpl::push_front<methods7, service::foo33>::type methods8;
        private: typedef boost::mpl::push_front<methods8, service::foo32>::type methods9;
        private: typedef boost::mpl::push_front<methods9, service::foo31>::type methods10;
        private: typedef boost::mpl::push_front<methods10, service::foo24>::type methods11;
        private: typedef boost::mpl::push_front<methods11, service::foo23>::type methods12;
        private: typedef boost::mpl::push_front<methods12, service::foo22>::type methods13;
        private: typedef boost::mpl::push_front<methods13, service::foo21>::type methods14;
        private: typedef boost::mpl::push_front<methods14, service::foo15>::type methods15;
        private: typedef boost::mpl::push_front<methods15, service::foo14>::type methods16;
        private: typedef boost::mpl::push_front<methods16, service::foo13>::type methods17;
        private: typedef boost::mpl::push_front<methods17, service::foo12_impl>::type methods18;
        private: typedef boost::mpl::push_front<methods18, service::foo12>::type methods19;
        private: typedef boost::mpl::push_front<methods19, service::foo11>::type methods20;

        public: typedef methods20::type methods;

        
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
            , rpcmethod_foo11_("/tests.Foo/foo11", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo12_("/tests.Foo/foo12", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo12_impl_("/tests.Foo/foo12_impl", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo13_("/tests.Foo/foo13", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo14_("/tests.Foo/foo14", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo15_("/tests.Foo/foo15", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo21_("/tests.Foo/foo21", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo22_("/tests.Foo/foo22", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo23_("/tests.Foo/foo23", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo24_("/tests.Foo/foo24", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo31_("/tests.Foo/foo31", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo32_("/tests.Foo/foo32", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo33_("/tests.Foo/foo33", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod__rd_foo33_("/tests.Foo/_rd_foo33", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo34_("/tests.Foo/foo34", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo41_("/tests.Foo/foo41", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo42_("/tests.Foo/foo42", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo43_("/tests.Foo/foo43", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_foo44_("/tests.Foo/foo44", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
            , rpcmethod_cq_("/tests.Foo/cq", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
        {
            BOOST_ASSERT(_scheduler);
        }

        void Asyncfoo11(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>());
            calldata->dispatch(rpcmethod_foo11_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo12(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>());
            calldata->dispatch(rpcmethod_foo12_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo12_impl(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>());
            calldata->dispatch(rpcmethod_foo12_impl_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo13(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>());
            calldata->dispatch(rpcmethod_foo13_, request);
        }
        void Asyncfoo13(const ::tests::BasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo13(::bond::bonded< ::tests::BasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo14(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>());
            calldata->dispatch(rpcmethod_foo14_, request);
        }
        void Asyncfoo14(const ::tests::dummy& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo14(::bond::bonded< ::tests::dummy>{request}, ::std::move(context));
        }

        void Asyncfoo15(const ::bond::bonded< ::tests2::OtherBasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests2::OtherBasicTypes, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>());
            calldata->dispatch(rpcmethod_foo15_, request);
        }
        void Asyncfoo15(const ::tests2::OtherBasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo15(::bond::bonded< ::tests2::OtherBasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo21(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo21_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo22(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo22_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo23(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo23_, request);
        }
        void Asyncfoo23(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo23(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }

        void Asyncfoo24(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::bond::Void>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo24_, request);
        }
        void Asyncfoo24(const ::tests::dummy& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo24(::bond::bonded< ::tests::dummy>{request}, cb, ::std::move(context));
        }

        void Asyncfoo31(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::BasicTypes>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo31_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo32(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::BasicTypes>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo32_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo33(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::tests::BasicTypes>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo33_, request);
        }
        void Asyncfoo33(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo33(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }

        void Async_rd_foo33(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::tests::BasicTypes>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod__rd_foo33_, request);
        }
        void Async_rd_foo33(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Async_rd_foo33(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }

        void Asyncfoo34(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::tests::BasicTypes>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo34_, request);
        }
        void Asyncfoo34(const ::tests::dummy& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo34(::bond::bonded< ::tests::dummy>{request}, cb, ::std::move(context));
        }

        void Asyncfoo41(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::dummy>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo41_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo42(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::dummy>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo42_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        void Asyncfoo43(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::tests::dummy>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo43_, request);
        }
        void Asyncfoo43(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo43(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }

        void Asyncfoo44(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::tests::dummy>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_foo44_, request);
        }
        void Asyncfoo44(const ::tests::dummy& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo44(::bond::bonded< ::tests::dummy>{request}, cb, ::std::move(context));
        }

        void Asynccq(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::BasicTypes>>(
                _channel,
                _ioManager,
                _scheduler,
                context ? ::std::move(context) : ::std::make_shared< ::grpc::ClientContext>(),
                cb);
            calldata->dispatch(rpcmethod_cq_, ::bond::bonded< ::bond::Void>{ ::bond::Void() });
        }

        Client(const Client&) = delete;
        Client& operator=(const Client&) = delete;

        Client(Client&&) = default;
        Client& operator=(Client&&) = default;

    private:
        ::std::shared_ptr< ::grpc::ChannelInterface> _channel;
        ::std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        ::bond::ext::gRPC::Scheduler _scheduler;

        const ::grpc::internal::RpcMethod rpcmethod_foo11_;
        const ::grpc::internal::RpcMethod rpcmethod_foo12_;
        const ::grpc::internal::RpcMethod rpcmethod_foo12_impl_;
        const ::grpc::internal::RpcMethod rpcmethod_foo13_;
        const ::grpc::internal::RpcMethod rpcmethod_foo14_;
        const ::grpc::internal::RpcMethod rpcmethod_foo15_;
        const ::grpc::internal::RpcMethod rpcmethod_foo21_;
        const ::grpc::internal::RpcMethod rpcmethod_foo22_;
        const ::grpc::internal::RpcMethod rpcmethod_foo23_;
        const ::grpc::internal::RpcMethod rpcmethod_foo24_;
        const ::grpc::internal::RpcMethod rpcmethod_foo31_;
        const ::grpc::internal::RpcMethod rpcmethod_foo32_;
        const ::grpc::internal::RpcMethod rpcmethod_foo33_;
        const ::grpc::internal::RpcMethod rpcmethod__rd_foo33_;
        const ::grpc::internal::RpcMethod rpcmethod_foo34_;
        const ::grpc::internal::RpcMethod rpcmethod_foo41_;
        const ::grpc::internal::RpcMethod rpcmethod_foo42_;
        const ::grpc::internal::RpcMethod rpcmethod_foo43_;
        const ::grpc::internal::RpcMethod rpcmethod_foo44_;
        const ::grpc::internal::RpcMethod rpcmethod_cq_;
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        explicit Service(const ::bond::ext::gRPC::Scheduler& scheduler)
            : ::bond::ext::gRPC::detail::service(
                scheduler,
                {
                    "/tests.Foo/foo11",
                    "/tests.Foo/foo12",
                    "/tests.Foo/foo12_impl",
                    "/tests.Foo/foo13",
                    "/tests.Foo/foo14",
                    "/tests.Foo/foo15",
                    "/tests.Foo/foo21",
                    "/tests.Foo/foo22",
                    "/tests.Foo/foo23",
                    "/tests.Foo/foo24",
                    "/tests.Foo/foo31",
                    "/tests.Foo/foo32",
                    "/tests.Foo/foo33",
                    "/tests.Foo/_rd_foo33",
                    "/tests.Foo/foo34",
                    "/tests.Foo/foo41",
                    "/tests.Foo/foo42",
                    "/tests.Foo/foo43",
                    "/tests.Foo/foo44",
                    "/tests.Foo/cq"
                })
        {}

        void start() override
        {
            _data.emplace(*this);
        }

        virtual void foo11(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::bond::Void>) = 0;
        virtual void foo12(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::bond::Void>) = 0;
        virtual void foo12_impl(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::bond::Void>) = 0;
        virtual void foo13(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::BasicTypes>, ::bond::Void>) = 0;
        virtual void foo14(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::dummy>, ::bond::Void>) = 0;
        virtual void foo15(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests2::OtherBasicTypes>, ::bond::Void>) = 0;
        virtual void foo21(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::bond::Void>) = 0;
        virtual void foo22(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::bond::Void>) = 0;
        virtual void foo23(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::BasicTypes>, ::bond::Void>) = 0;
        virtual void foo24(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::dummy>, ::bond::Void>) = 0;
        virtual void foo31(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::tests::BasicTypes>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::tests::BasicTypes>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::BasicTypes>, ::tests::BasicTypes>) = 0;
        virtual void _rd_foo33(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::BasicTypes>, ::tests::BasicTypes>) = 0;
        virtual void foo34(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::dummy>, ::tests::BasicTypes>) = 0;
        virtual void foo41(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::tests::dummy>) = 0;
        virtual void foo42(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::tests::dummy>) = 0;
        virtual void foo43(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::BasicTypes>, ::tests::dummy>) = 0;
        virtual void foo44(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::dummy>, ::tests::dummy>) = 0;
        virtual void cq(::bond::ext::gRPC::unary_call< ::bond::bonded< ::bond::Void>, ::tests::BasicTypes>) = 0;

    private:
        struct data
        {
            explicit data(Service& s)
                : _s(s)
            {}

            Service& _s;
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo11> _m0{ _s, 0, ::std::bind(&Service::foo11, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo12> _m1{ _s, 1, ::std::bind(&Service::foo12, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo12_impl> _m2{ _s, 2, ::std::bind(&Service::foo12_impl, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo13> _m3{ _s, 3, ::std::bind(&Service::foo13, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo14> _m4{ _s, 4, ::std::bind(&Service::foo14, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo15> _m5{ _s, 5, ::std::bind(&Service::foo15, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo21> _m6{ _s, 6, ::std::bind(&Service::foo21, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo22> _m7{ _s, 7, ::std::bind(&Service::foo22, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo23> _m8{ _s, 8, ::std::bind(&Service::foo23, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo24> _m9{ _s, 9, ::std::bind(&Service::foo24, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo31> _m10{ _s, 10, ::std::bind(&Service::foo31, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo32> _m11{ _s, 11, ::std::bind(&Service::foo32, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo33> _m12{ _s, 12, ::std::bind(&Service::foo33, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::_rd_foo33> _m13{ _s, 13, ::std::bind(&Service::_rd_foo33, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo34> _m14{ _s, 14, ::std::bind(&Service::foo34, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo41> _m15{ _s, 15, ::std::bind(&Service::foo41, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo42> _m16{ _s, 16, ::std::bind(&Service::foo42, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo43> _m17{ _s, 17, ::std::bind(&Service::foo43, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo44> _m18{ _s, 18, ::std::bind(&Service::foo44, &_s, ::std::placeholders::_1) };
            ::bond::ext::gRPC::detail::service::Method<Schema::service::cq> _m19{ _s, 19, ::std::bind(&Service::cq, &_s, ::std::placeholders::_1) };
        };

        ::boost::optional<data> _data;
    };
};




} // namespace tests

