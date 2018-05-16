
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
#include <bond/ext/grpc/detail/client.h>
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

    class Client : public ::bond::ext::gRPC::detail::client
    {
    public:
        using ::bond::ext::gRPC::detail::client::client;

        void Asyncfoo(const ::bond::bonded< ::tests::Param>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::Result>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo, request, std::move(context), cb);
        }
        void Asyncfoo(const ::tests::Param& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::Result>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo(::bond::bonded< ::tests::Param>{request}, cb, ::std::move(context));
        }

    private:
        const ::grpc::internal::RpcMethod _mfoo{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo") };
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        explicit Service(const ::bond::ext::gRPC::Scheduler& scheduler = {})
            : ::bond::ext::gRPC::detail::service(
                scheduler,
                {
                    "/tests.Foo/foo"
                })
        {}

        void start() override
        {
            _data.emplace(*this);
        }

        virtual void foo(::bond::ext::gRPC::unary_call< ::bond::bonded< ::tests::Param>, ::tests::Result>) = 0;

    private:
        struct data
        {
            explicit data(Service& s)
                : _s(s)
            {}

            Service& _s;
            ::bond::ext::gRPC::detail::service::Method<Schema::service::foo> _m0{ _s, 0, ::std::bind(&Service::foo, &_s, ::std::placeholders::_1) };
        };

        ::boost::optional<data> _data;
    };
};




} // namespace tests

