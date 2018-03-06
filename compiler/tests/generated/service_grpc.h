
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
                &_rd_foo11->_receivedCall->context(),
                &_rd_foo11->_receivedCall->request(),
                &_rd_foo11->_receivedCall->responder(),
                cq0,
                &_rd_foo11.get());
            this->queue_receive(
                1,
                &_rd_foo12->_receivedCall->context(),
                &_rd_foo12->_receivedCall->request(),
                &_rd_foo12->_receivedCall->responder(),
                cq0,
                &_rd_foo12.get());
            this->queue_receive(
                2,
                &_rd_foo12_impl->_receivedCall->context(),
                &_rd_foo12_impl->_receivedCall->request(),
                &_rd_foo12_impl->_receivedCall->responder(),
                cq0,
                &_rd_foo12_impl.get());
            this->queue_receive(
                3,
                &_rd_foo13->_receivedCall->context(),
                &_rd_foo13->_receivedCall->request(),
                &_rd_foo13->_receivedCall->responder(),
                cq0,
                &_rd_foo13.get());
            this->queue_receive(
                4,
                &_rd_foo14->_receivedCall->context(),
                &_rd_foo14->_receivedCall->request(),
                &_rd_foo14->_receivedCall->responder(),
                cq0,
                &_rd_foo14.get());
            this->queue_receive(
                5,
                &_rd_foo15->_receivedCall->context(),
                &_rd_foo15->_receivedCall->request(),
                &_rd_foo15->_receivedCall->responder(),
                cq0,
                &_rd_foo15.get());
            this->queue_receive(
                6,
                &_rd_foo21->_receivedCall->context(),
                &_rd_foo21->_receivedCall->request(),
                &_rd_foo21->_receivedCall->responder(),
                cq0,
                &_rd_foo21.get());
            this->queue_receive(
                7,
                &_rd_foo22->_receivedCall->context(),
                &_rd_foo22->_receivedCall->request(),
                &_rd_foo22->_receivedCall->responder(),
                cq0,
                &_rd_foo22.get());
            this->queue_receive(
                8,
                &_rd_foo23->_receivedCall->context(),
                &_rd_foo23->_receivedCall->request(),
                &_rd_foo23->_receivedCall->responder(),
                cq0,
                &_rd_foo23.get());
            this->queue_receive(
                9,
                &_rd_foo24->_receivedCall->context(),
                &_rd_foo24->_receivedCall->request(),
                &_rd_foo24->_receivedCall->responder(),
                cq0,
                &_rd_foo24.get());
            this->queue_receive(
                10,
                &_rd_foo31->_receivedCall->context(),
                &_rd_foo31->_receivedCall->request(),
                &_rd_foo31->_receivedCall->responder(),
                cq0,
                &_rd_foo31.get());
            this->queue_receive(
                11,
                &_rd_foo32->_receivedCall->context(),
                &_rd_foo32->_receivedCall->request(),
                &_rd_foo32->_receivedCall->responder(),
                cq0,
                &_rd_foo32.get());
            this->queue_receive(
                12,
                &_rd_foo330->_receivedCall->context(),
                &_rd_foo330->_receivedCall->request(),
                &_rd_foo330->_receivedCall->responder(),
                cq0,
                &_rd_foo330.get());
            this->queue_receive(
                13,
                &_rd__rd_foo33->_receivedCall->context(),
                &_rd__rd_foo33->_receivedCall->request(),
                &_rd__rd_foo33->_receivedCall->responder(),
                cq0,
                &_rd__rd_foo33.get());
            this->queue_receive(
                14,
                &_rd_foo34->_receivedCall->context(),
                &_rd_foo34->_receivedCall->request(),
                &_rd_foo34->_receivedCall->responder(),
                cq0,
                &_rd_foo34.get());
            this->queue_receive(
                15,
                &_rd_foo41->_receivedCall->context(),
                &_rd_foo41->_receivedCall->request(),
                &_rd_foo41->_receivedCall->responder(),
                cq0,
                &_rd_foo41.get());
            this->queue_receive(
                16,
                &_rd_foo42->_receivedCall->context(),
                &_rd_foo42->_receivedCall->request(),
                &_rd_foo42->_receivedCall->responder(),
                cq0,
                &_rd_foo42.get());
            this->queue_receive(
                17,
                &_rd_foo43->_receivedCall->context(),
                &_rd_foo43->_receivedCall->request(),
                &_rd_foo43->_receivedCall->responder(),
                cq0,
                &_rd_foo43.get());
            this->queue_receive(
                18,
                &_rd_foo44->_receivedCall->context(),
                &_rd_foo44->_receivedCall->request(),
                &_rd_foo44->_receivedCall->responder(),
                cq0,
                &_rd_foo44.get());
            this->queue_receive(
                19,
                &_rd_cq->_receivedCall->context(),
                &_rd_cq->_receivedCall->request(),
                &_rd_cq->_receivedCall->responder(),
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

struct Foo::Schema
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



} // namespace tests

