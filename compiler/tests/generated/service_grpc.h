
#pragma once

#include "service_reflection.h"
#include "service_types.h"
#include "basic_types_grpc.h"
#include "namespace_basic_types_grpc.h"
#include <bond/core/bonded.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/client_callback.h>
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

        void Asyncfoo11(::std::shared_ptr< ::grpc::ClientContext> context);
        void Asyncfoo11()
        {
            Asyncfoo11(::std::make_shared< ::grpc::ClientContext>());
        }

        void Asyncfoo12(::std::shared_ptr< ::grpc::ClientContext> context);
        void Asyncfoo12()
        {
            Asyncfoo12(::std::make_shared< ::grpc::ClientContext>());
        }

        void Asyncfoo12_impl(::std::shared_ptr< ::grpc::ClientContext> context);
        void Asyncfoo12_impl()
        {
            Asyncfoo12_impl(::std::make_shared< ::grpc::ClientContext>());
        }

        void Asyncfoo13(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::BasicTypes>& request);
        void Asyncfoo13(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::BasicTypes& request)
        {
            Asyncfoo13(context, ::bond::bonded< ::tests::BasicTypes>{request});
        }
        void Asyncfoo13(const ::bond::bonded< ::tests::BasicTypes>& request)
        {
            Asyncfoo13(::std::make_shared< ::grpc::ClientContext>(), request);
        }
        void Asyncfoo13(const ::tests::BasicTypes& request)
        {
            Asyncfoo13(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::BasicTypes>{request});
        }

        void Asyncfoo14(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::dummy>& request);
        void Asyncfoo14(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::dummy& request)
        {
            Asyncfoo14(context, ::bond::bonded< ::tests::dummy>{request});
        }
        void Asyncfoo14(const ::bond::bonded< ::tests::dummy>& request)
        {
            Asyncfoo14(::std::make_shared< ::grpc::ClientContext>(), request);
        }
        void Asyncfoo14(const ::tests::dummy& request)
        {
            Asyncfoo14(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::dummy>{request});
        }

        void Asyncfoo15(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests2::OtherBasicTypes>& request);
        void Asyncfoo15(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests2::OtherBasicTypes& request)
        {
            Asyncfoo15(context, ::bond::bonded< ::tests2::OtherBasicTypes>{request});
        }
        void Asyncfoo15(const ::bond::bonded< ::tests2::OtherBasicTypes>& request)
        {
            Asyncfoo15(::std::make_shared< ::grpc::ClientContext>(), request);
        }
        void Asyncfoo15(const ::tests2::OtherBasicTypes& request)
        {
            Asyncfoo15(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests2::OtherBasicTypes>{request});
        }

        void Asyncfoo21(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb);
        void Asyncfoo21(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo21(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo22(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb);
        void Asyncfoo22(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo22(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo23(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb);
        void Asyncfoo23(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo23(context, ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }
        void Asyncfoo23(const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo23(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo23(const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo23(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }

        void Asyncfoo24(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::dummy>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb);
        void Asyncfoo24(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::dummy& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo24(context, ::bond::bonded< ::tests::dummy>{request}, cb);
        }
        void Asyncfoo24(const ::bond::bonded< ::tests::dummy>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo24(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo24(const ::tests::dummy& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
        {
            Asyncfoo24(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::dummy>{request}, cb);
        }

        void Asyncfoo31(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb);
        void Asyncfoo31(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo31(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo32(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb);
        void Asyncfoo32(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo32(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo33(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb);
        void Asyncfoo33(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo33(context, ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }
        void Asyncfoo33(const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo33(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo33(const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo33(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }

        void Async_rd_foo33(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb);
        void Async_rd_foo33(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Async_rd_foo33(context, ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }
        void Async_rd_foo33(const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Async_rd_foo33(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Async_rd_foo33(const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Async_rd_foo33(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }

        void Asyncfoo34(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::dummy>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb);
        void Asyncfoo34(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::dummy& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo34(context, ::bond::bonded< ::tests::dummy>{request}, cb);
        }
        void Asyncfoo34(const ::bond::bonded< ::tests::dummy>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo34(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo34(const ::tests::dummy& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asyncfoo34(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::dummy>{request}, cb);
        }

        void Asyncfoo41(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb);
        void Asyncfoo41(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo41(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo42(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb);
        void Asyncfoo42(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo42(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        void Asyncfoo43(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb);
        void Asyncfoo43(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo43(context, ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }
        void Asyncfoo43(const ::bond::bonded< ::tests::BasicTypes>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo43(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo43(const ::tests::BasicTypes& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo43(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::BasicTypes>{request}, cb);
        }

        void Asyncfoo44(::std::shared_ptr< ::grpc::ClientContext> context, const ::bond::bonded< ::tests::dummy>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb);
        void Asyncfoo44(::std::shared_ptr< ::grpc::ClientContext> context, const ::tests::dummy& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo44(context, ::bond::bonded< ::tests::dummy>{request}, cb);
        }
        void Asyncfoo44(const ::bond::bonded< ::tests::dummy>& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo44(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Asyncfoo44(const ::tests::dummy& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
        {
            Asyncfoo44(::std::make_shared< ::grpc::ClientContext>(), ::bond::bonded< ::tests::dummy>{request}, cb);
        }

        void Asynccq(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb);
        void Asynccq(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
        {
            Asynccq(::std::make_shared< ::grpc::ClientContext>(), cb);
        }

        ClientCore(const ClientCore&) = delete;
        ClientCore& operator=(const ClientCore&) = delete;

        ClientCore(ClientCore&&) = default;
        ClientCore& operator=(ClientCore&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> _channel;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        std::shared_ptr<TThreadPool> _threadPool;

        const ::grpc::RpcMethod rpcmethod_foo11_;

        const ::grpc::RpcMethod rpcmethod_foo12_;

        const ::grpc::RpcMethod rpcmethod_foo12_impl_;

        const ::grpc::RpcMethod rpcmethod_foo13_;

        const ::grpc::RpcMethod rpcmethod_foo14_;

        const ::grpc::RpcMethod rpcmethod_foo15_;

        const ::grpc::RpcMethod rpcmethod_foo21_;

        const ::grpc::RpcMethod rpcmethod_foo22_;

        const ::grpc::RpcMethod rpcmethod_foo23_;

        const ::grpc::RpcMethod rpcmethod_foo24_;

        const ::grpc::RpcMethod rpcmethod_foo31_;

        const ::grpc::RpcMethod rpcmethod_foo32_;

        const ::grpc::RpcMethod rpcmethod_foo33_;

        const ::grpc::RpcMethod rpcmethod__rd_foo33_;

        const ::grpc::RpcMethod rpcmethod_foo34_;

        const ::grpc::RpcMethod rpcmethod_foo41_;

        const ::grpc::RpcMethod rpcmethod_foo42_;

        const ::grpc::RpcMethod rpcmethod_foo43_;

        const ::grpc::RpcMethod rpcmethod_foo44_;

        const ::grpc::RpcMethod rpcmethod_cq_;
    };

    using Client = ClientCore< ::bond::ext::gRPC::thread_pool>;

    template <typename TThreadPool>
    class ServiceCore : public ::bond::ext::gRPC::detail::service<TThreadPool>
    {
    public:
        ServiceCore()
        {
            this->AddMethod("/tests.Foo/foo11");
            this->AddMethod("/tests.Foo/foo12");
            this->AddMethod("/tests.Foo/foo12_impl");
            this->AddMethod("/tests.Foo/foo13");
            this->AddMethod("/tests.Foo/foo14");
            this->AddMethod("/tests.Foo/foo15");
            this->AddMethod("/tests.Foo/foo21");
            this->AddMethod("/tests.Foo/foo22");
            this->AddMethod("/tests.Foo/foo23");
            this->AddMethod("/tests.Foo/foo24");
            this->AddMethod("/tests.Foo/foo31");
            this->AddMethod("/tests.Foo/foo32");
            this->AddMethod("/tests.Foo/foo33");
            this->AddMethod("/tests.Foo/_rd_foo33");
            this->AddMethod("/tests.Foo/foo34");
            this->AddMethod("/tests.Foo/foo41");
            this->AddMethod("/tests.Foo/foo42");
            this->AddMethod("/tests.Foo/foo43");
            this->AddMethod("/tests.Foo/foo44");
            this->AddMethod("/tests.Foo/cq");
        }

        virtual ~ServiceCore() { }
        virtual void start(
            ::grpc::ServerCompletionQueue* cq0,
            std::shared_ptr<TThreadPool> tp) override
        {
            BOOST_ASSERT(cq0);
            BOOST_ASSERT(tp);

            _rd_foo11.emplace(
                this,
                0,
                cq0,
                tp,
                std::bind(&ServiceCore::foo11, this, std::placeholders::_1));
            _rd_foo12.emplace(
                this,
                1,
                cq0,
                tp,
                std::bind(&ServiceCore::foo12, this, std::placeholders::_1));
            _rd_foo12_impl.emplace(
                this,
                2,
                cq0,
                tp,
                std::bind(&ServiceCore::foo12_impl, this, std::placeholders::_1));
            _rd_foo13.emplace(
                this,
                3,
                cq0,
                tp,
                std::bind(&ServiceCore::foo13, this, std::placeholders::_1));
            _rd_foo14.emplace(
                this,
                4,
                cq0,
                tp,
                std::bind(&ServiceCore::foo14, this, std::placeholders::_1));
            _rd_foo15.emplace(
                this,
                5,
                cq0,
                tp,
                std::bind(&ServiceCore::foo15, this, std::placeholders::_1));
            _rd_foo21.emplace(
                this,
                6,
                cq0,
                tp,
                std::bind(&ServiceCore::foo21, this, std::placeholders::_1));
            _rd_foo22.emplace(
                this,
                7,
                cq0,
                tp,
                std::bind(&ServiceCore::foo22, this, std::placeholders::_1));
            _rd_foo23.emplace(
                this,
                8,
                cq0,
                tp,
                std::bind(&ServiceCore::foo23, this, std::placeholders::_1));
            _rd_foo24.emplace(
                this,
                9,
                cq0,
                tp,
                std::bind(&ServiceCore::foo24, this, std::placeholders::_1));
            _rd_foo31.emplace(
                this,
                10,
                cq0,
                tp,
                std::bind(&ServiceCore::foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(
                this,
                11,
                cq0,
                tp,
                std::bind(&ServiceCore::foo32, this, std::placeholders::_1));
            _rd_foo330.emplace(
                this,
                12,
                cq0,
                tp,
                std::bind(&ServiceCore::foo33, this, std::placeholders::_1));
            _rd__rd_foo33.emplace(
                this,
                13,
                cq0,
                tp,
                std::bind(&ServiceCore::_rd_foo33, this, std::placeholders::_1));
            _rd_foo34.emplace(
                this,
                14,
                cq0,
                tp,
                std::bind(&ServiceCore::foo34, this, std::placeholders::_1));
            _rd_foo41.emplace(
                this,
                15,
                cq0,
                tp,
                std::bind(&ServiceCore::foo41, this, std::placeholders::_1));
            _rd_foo42.emplace(
                this,
                16,
                cq0,
                tp,
                std::bind(&ServiceCore::foo42, this, std::placeholders::_1));
            _rd_foo43.emplace(
                this,
                17,
                cq0,
                tp,
                std::bind(&ServiceCore::foo43, this, std::placeholders::_1));
            _rd_foo44.emplace(
                this,
                18,
                cq0,
                tp,
                std::bind(&ServiceCore::foo44, this, std::placeholders::_1));
            _rd_cq.emplace(
                this,
                19,
                cq0,
                tp,
                std::bind(&ServiceCore::cq, this, std::placeholders::_1));

            this->queue_receive(
                0,
                &_rd_foo11->_receivedCall->_context,
                &_rd_foo11->_receivedCall->_request,
                &_rd_foo11->_receivedCall->_responder,
                cq0,
                &_rd_foo11.get());
            this->queue_receive(
                1,
                &_rd_foo12->_receivedCall->_context,
                &_rd_foo12->_receivedCall->_request,
                &_rd_foo12->_receivedCall->_responder,
                cq0,
                &_rd_foo12.get());
            this->queue_receive(
                2,
                &_rd_foo12_impl->_receivedCall->_context,
                &_rd_foo12_impl->_receivedCall->_request,
                &_rd_foo12_impl->_receivedCall->_responder,
                cq0,
                &_rd_foo12_impl.get());
            this->queue_receive(
                3,
                &_rd_foo13->_receivedCall->_context,
                &_rd_foo13->_receivedCall->_request,
                &_rd_foo13->_receivedCall->_responder,
                cq0,
                &_rd_foo13.get());
            this->queue_receive(
                4,
                &_rd_foo14->_receivedCall->_context,
                &_rd_foo14->_receivedCall->_request,
                &_rd_foo14->_receivedCall->_responder,
                cq0,
                &_rd_foo14.get());
            this->queue_receive(
                5,
                &_rd_foo15->_receivedCall->_context,
                &_rd_foo15->_receivedCall->_request,
                &_rd_foo15->_receivedCall->_responder,
                cq0,
                &_rd_foo15.get());
            this->queue_receive(
                6,
                &_rd_foo21->_receivedCall->_context,
                &_rd_foo21->_receivedCall->_request,
                &_rd_foo21->_receivedCall->_responder,
                cq0,
                &_rd_foo21.get());
            this->queue_receive(
                7,
                &_rd_foo22->_receivedCall->_context,
                &_rd_foo22->_receivedCall->_request,
                &_rd_foo22->_receivedCall->_responder,
                cq0,
                &_rd_foo22.get());
            this->queue_receive(
                8,
                &_rd_foo23->_receivedCall->_context,
                &_rd_foo23->_receivedCall->_request,
                &_rd_foo23->_receivedCall->_responder,
                cq0,
                &_rd_foo23.get());
            this->queue_receive(
                9,
                &_rd_foo24->_receivedCall->_context,
                &_rd_foo24->_receivedCall->_request,
                &_rd_foo24->_receivedCall->_responder,
                cq0,
                &_rd_foo24.get());
            this->queue_receive(
                10,
                &_rd_foo31->_receivedCall->_context,
                &_rd_foo31->_receivedCall->_request,
                &_rd_foo31->_receivedCall->_responder,
                cq0,
                &_rd_foo31.get());
            this->queue_receive(
                11,
                &_rd_foo32->_receivedCall->_context,
                &_rd_foo32->_receivedCall->_request,
                &_rd_foo32->_receivedCall->_responder,
                cq0,
                &_rd_foo32.get());
            this->queue_receive(
                12,
                &_rd_foo330->_receivedCall->_context,
                &_rd_foo330->_receivedCall->_request,
                &_rd_foo330->_receivedCall->_responder,
                cq0,
                &_rd_foo330.get());
            this->queue_receive(
                13,
                &_rd__rd_foo33->_receivedCall->_context,
                &_rd__rd_foo33->_receivedCall->_request,
                &_rd__rd_foo33->_receivedCall->_responder,
                cq0,
                &_rd__rd_foo33.get());
            this->queue_receive(
                14,
                &_rd_foo34->_receivedCall->_context,
                &_rd_foo34->_receivedCall->_request,
                &_rd_foo34->_receivedCall->_responder,
                cq0,
                &_rd_foo34.get());
            this->queue_receive(
                15,
                &_rd_foo41->_receivedCall->_context,
                &_rd_foo41->_receivedCall->_request,
                &_rd_foo41->_receivedCall->_responder,
                cq0,
                &_rd_foo41.get());
            this->queue_receive(
                16,
                &_rd_foo42->_receivedCall->_context,
                &_rd_foo42->_receivedCall->_request,
                &_rd_foo42->_receivedCall->_responder,
                cq0,
                &_rd_foo42.get());
            this->queue_receive(
                17,
                &_rd_foo43->_receivedCall->_context,
                &_rd_foo43->_receivedCall->_request,
                &_rd_foo43->_receivedCall->_responder,
                cq0,
                &_rd_foo43.get());
            this->queue_receive(
                18,
                &_rd_foo44->_receivedCall->_context,
                &_rd_foo44->_receivedCall->_request,
                &_rd_foo44->_receivedCall->_responder,
                cq0,
                &_rd_foo44.get());
            this->queue_receive(
                19,
                &_rd_cq->_receivedCall->_context,
                &_rd_cq->_receivedCall->_request,
                &_rd_cq->_receivedCall->_responder,
                cq0,
                &_rd_cq.get());
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
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::bond::Void, TThreadPool>> _rd_foo11;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::bond::Void, TThreadPool>> _rd_foo12;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::bond::Void, TThreadPool>> _rd_foo12_impl;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::BasicTypes>, ::bond::Void, TThreadPool>> _rd_foo13;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::dummy>, ::bond::Void, TThreadPool>> _rd_foo14;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests2::OtherBasicTypes>, ::bond::Void, TThreadPool>> _rd_foo15;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::bond::Void, TThreadPool>> _rd_foo21;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::bond::Void, TThreadPool>> _rd_foo22;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::BasicTypes>, ::bond::Void, TThreadPool>> _rd_foo23;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::dummy>, ::bond::Void, TThreadPool>> _rd_foo24;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::tests::BasicTypes, TThreadPool>> _rd_foo31;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::tests::BasicTypes, TThreadPool>> _rd_foo32;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::BasicTypes>, ::tests::BasicTypes, TThreadPool>> _rd_foo330;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::BasicTypes>, ::tests::BasicTypes, TThreadPool>> _rd__rd_foo33;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::dummy>, ::tests::BasicTypes, TThreadPool>> _rd_foo34;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::tests::dummy, TThreadPool>> _rd_foo41;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::tests::dummy, TThreadPool>> _rd_foo42;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::BasicTypes>, ::tests::dummy, TThreadPool>> _rd_foo43;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::tests::dummy>, ::tests::dummy, TThreadPool>> _rd_foo44;
        ::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< ::bond::bonded< ::bond::Void>, ::tests::BasicTypes, TThreadPool>> _rd_cq;
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
    , rpcmethod_foo11_("/tests.Foo/foo11", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo12_("/tests.Foo/foo12", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo12_impl_("/tests.Foo/foo12_impl", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo13_("/tests.Foo/foo13", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo14_("/tests.Foo/foo14", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo15_("/tests.Foo/foo15", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo21_("/tests.Foo/foo21", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo22_("/tests.Foo/foo22", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo23_("/tests.Foo/foo23", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo24_("/tests.Foo/foo24", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo31_("/tests.Foo/foo31", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_("/tests.Foo/foo32", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_("/tests.Foo/foo33", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod__rd_foo33_("/tests.Foo/_rd_foo33", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo34_("/tests.Foo/foo34", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo41_("/tests.Foo/foo41", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo42_("/tests.Foo/foo42", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo43_("/tests.Foo/foo43", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo44_("/tests.Foo/foo44", ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_cq_("/tests.Foo/cq", ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo11(
    ::std::shared_ptr< ::grpc::ClientContext> context
    )
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_foo11_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo12(
    ::std::shared_ptr< ::grpc::ClientContext> context
    )
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_foo12_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo12_impl(
    ::std::shared_ptr< ::grpc::ClientContext> context
    )
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_foo12_impl_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo13(
    ::std::shared_ptr< ::grpc::ClientContext> context
    , const ::bond::bonded< ::tests::BasicTypes>& request)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_foo13_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo14(
    ::std::shared_ptr< ::grpc::ClientContext> context
    , const ::bond::bonded< ::tests::dummy>& request)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_foo14_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo15(
    ::std::shared_ptr< ::grpc::ClientContext> context
    , const ::bond::bonded< ::tests2::OtherBasicTypes>& request)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests2::OtherBasicTypes, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_foo15_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo21(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo21_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo22(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo22_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo23(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::BasicTypes>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo23_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo24(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::dummy>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::bond::Void>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::bond::Void, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo24_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo31(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::BasicTypes, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo31_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo32(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::BasicTypes, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo32_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo33(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::BasicTypes>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::tests::BasicTypes, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo33_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Async_rd_foo33(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::BasicTypes>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::tests::BasicTypes, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod__rd_foo33_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo34(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::dummy>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::tests::BasicTypes, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo34_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo41(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::dummy, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo41_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo42(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::dummy, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo42_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo43(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::BasicTypes>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::BasicTypes, ::tests::dummy, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo43_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asyncfoo44(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    const ::bond::bonded< ::tests::dummy>& request,
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::dummy>>)>& cb)
{
    
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::tests::dummy, ::tests::dummy, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_foo44_, request);
}

template <typename TThreadPool>
inline void Foo::ClientCore<TThreadPool>::Asynccq(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>>)>& cb)
{
    auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::Void, ::tests::BasicTypes, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_cq_, request);
}


} // namespace tests

