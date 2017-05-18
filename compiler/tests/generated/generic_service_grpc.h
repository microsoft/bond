
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"


#include <bond/comm/message.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/detail/service_call_data.h>

#include <boost/optional/optional.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100 4267)
#endif

#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/method_handler_impl.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/service_type.h>
#include <grpc++/impl/codegen/status.h>
#include <grpc++/impl/codegen/stub_options.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace tests
{

class Foo final
{
public:
    class StubInterface
    {
    public:
        virtual ~StubInterface() {}

        virtual ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<void>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo31Raw(context, request, cq));
        }

        virtual ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<Payload>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>>(Asyncfoo32Raw(context, request, cq));
        }

        virtual ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<Payload>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>>(Asyncfoo33Raw(context, request, cq));
        }

    private:
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) = 0;
    };

    class Stub final : public StubInterface
    {
    public:
        Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);

        ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<void>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo31Raw(context, request, cq));
        }

        ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<Payload>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>>(Asyncfoo32Raw(context, request, cq));
        }

        ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<Payload>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>>(Asyncfoo33Raw(context, request, cq));
        }

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo31_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo32_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo33_;
    };

    static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            AddMethod("/tests.Foo/foo31");
            AddMethod("/tests.Foo/foo32");
            AddMethod("/tests.Foo/foo33");
        }

        virtual ~Service() { }
        virtual void start(::grpc::ServerCompletionQueue* cq) override
        {
            BOOST_ASSERT(cq);

            _rd_foo31.emplace(this, 0, cq, std::bind(&Service::foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(this, 1, cq, std::bind(&Service::foo32, this, std::placeholders::_1));
            _rd_foo33.emplace(this, 2, cq, std::bind(&Service::foo33, this, std::placeholders::_1));

            queue_receive(0, &_rd_foo31->_receivedCall->_context, &_rd_foo31->_receivedCall->_request, &_rd_foo31->_receivedCall->_responder, cq, &_rd_foo31.get());
            queue_receive(1, &_rd_foo32->_receivedCall->_context, &_rd_foo32->_receivedCall->_request, &_rd_foo32->_receivedCall->_responder, cq, &_rd_foo32.get());
            queue_receive(2, &_rd_foo33->_receivedCall->_context, &_rd_foo33->_receivedCall->_request, &_rd_foo33->_receivedCall->_responder, cq, &_rd_foo33.get());
        }

        virtual void foo31(::bond::ext::gRPC::unary_call<::bond::comm::message<Payload>, ::bond::comm::message<void>>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message<Payload>>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call<::bond::comm::message<Payload>, ::bond::comm::message<Payload>>) = 0;

    private:
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<Payload>, ::bond::comm::message<void>>> _rd_foo31;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>, ::bond::comm::message<Payload>>> _rd_foo32;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<Payload>, ::bond::comm::message<Payload>>> _rd_foo33;
    };
};

} // namespace tests

