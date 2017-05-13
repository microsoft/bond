
#pragma once

#include "service_attributes_reflection.h"
#include "service_attributes_types.h"


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

        virtual ::grpc::Status foo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::bond::comm::message< ::tests::Result>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::Result>>> Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::Result>>>(AsyncfooRaw(context, request, cq));
        }

    private:
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::Result>>* AsyncfooRaw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq) = 0;
    };

    class Stub final : public StubInterface
    {
    public:
        Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);

        ::grpc::Status foo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::bond::comm::message< ::tests::Result>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>> Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>>(AsyncfooRaw(context, request, cq));
        }

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>* AsyncfooRaw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo_;
    };

    static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            AddMethod("/tests.Foo/foo");
        }

        virtual ~Service() { }
        virtual void start(::grpc::ServerCompletionQueue* cq) override
        {
            BOOST_ASSERT(cq);

            _rd_foo.emplace(this, 0, cq, std::bind(&foo, this, std::placeholders::_1));

            queue_receive(0, &_rd_foo->_receivedCall->_context, &_rd_foo->_receivedCall->_request, &_rd_foo->_receivedCall->_responder, cq, &_rd_foo.get());
        }

        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result>) = 0;

    private:
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::Param>>, ::bond::comm::message< ::tests::Result>>>> _rd_foo;
    };
};

} // namespace tests

