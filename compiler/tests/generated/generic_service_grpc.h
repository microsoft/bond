
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"

#include <bond/core/bonded.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/io_manager.h>
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
    class FooClient
    {
    public:
        FooClient(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager);

        void Asyncfoo31(::grpc::ClientContext* context, const ::bond::bonded<Payload>& request, std::function<void(const ::bond::bonded<void>&, const ::grpc::Status&)> cb);

        void Asyncfoo32(::grpc::ClientContext* context, const ::bond::bonded<void>& request, std::function<void(const ::bond::bonded<Payload>&, const ::grpc::Status&)> cb);

        void Asyncfoo33(::grpc::ClientContext* context, const ::bond::bonded<Payload>& request, std::function<void(const ::bond::bonded<Payload>&, const ::grpc::Status&)> cb);

        FooClient(const FooClient&) = delete;
        FooClient& operator=(const FooClient&) = delete;

        FooClient(FooClient&&) = default;
        FooClient& operator=(FooClient&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager_;

        const ::grpc::RpcMethod rpcmethod_foo31_;

        const ::grpc::RpcMethod rpcmethod_foo32_;

        const ::grpc::RpcMethod rpcmethod_foo33_;
    };

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

        virtual void foo31(::bond::ext::gRPC::unary_call<::bond::bonded<Payload>, ::bond::bonded<void>>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call<::bond::bonded<void>, ::bond::bonded<Payload>>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call<::bond::bonded<Payload>, ::bond::bonded<Payload>>) = 0;

    private:
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::bonded<Payload>, ::bond::bonded<void>>> _rd_foo31;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::bonded<void>, ::bond::bonded<Payload>>> _rd_foo32;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::bonded<Payload>, ::bond::bonded<Payload>>> _rd_foo33;
    };
};

} // namespace tests

