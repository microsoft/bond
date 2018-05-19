
#pragma once

#include "service_attributes_reflection.h"
#include "service_attributes_types.h"


#include <bond/core/bonded.h>
#include <bond/ext/grpc/reflection.h>
#include <bond/ext/grpc/detail/client.h>
#include <bond/ext/grpc/detail/service.h>

#include <boost/optional/optional.hpp>
#include <functional>
#include <memory>

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
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::Param, ::tests::Result, &s_foo_metadata> {} foo;
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
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::tests::Result>> Asyncfoo(const ::bond::bonded< ::tests::Param>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::Result>(_mfoo, std::move(context), request);
        }
        void Asyncfoo(const ::tests::Param& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::Result>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo(::bond::bonded< ::tests::Param>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::Result>> Asyncfoo(const ::tests::Param& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo(::bond::bonded< ::tests::Param>{request}, ::std::move(context));
        }

    private:
        const ::bond::ext::gRPC::detail::client::Method _mfoo{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo") };
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        explicit Service(const ::bond::ext::gRPC::Scheduler& scheduler)
            : ::bond::ext::gRPC::detail::service(
                scheduler,
                {
                    "/tests.Foo/foo"
                })
        {}

        virtual void foo(::bond::ext::gRPC::unary_call< ::tests::Param, ::tests::Result>) = 0;

    private:
        void start() override
        {
            _data.emplace(*this);
        }

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

