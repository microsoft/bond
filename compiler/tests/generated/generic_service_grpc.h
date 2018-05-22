
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"

#include <bond/core/bond_reflection.h>
#include <bond/core/bonded.h>
#include <bond/ext/grpc/reflection.h>
#include <bond/ext/grpc/detail/client.h>
#include <bond/ext/grpc/detail/service.h>

#include <boost/optional/optional.hpp>
#include <functional>
#include <memory>

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
            typedef struct : ::bond::ext::grpc::reflection::MethodTemplate<Foo, Payload, void, &s_foo31_metadata> {} foo31;
            typedef struct : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, Payload, &s_foo32_metadata> {} foo32;
            typedef struct : ::bond::ext::grpc::reflection::MethodTemplate<Foo, Payload, Payload, &s_foo33_metadata> {} foo33;
            typedef struct : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::SomeBox<int32_t>, void, &s_ConsumesGeneric1_metadata> {} ConsumesGeneric1;
            typedef struct : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::SomeBox<std::vector<int32_t> >, void, &s_ConsumesGeneric2_metadata> {} ConsumesGeneric2;
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

    class Client : public ::bond::ext::grpc::detail::client
    {
    public:
        using ::bond::ext::grpc::detail::client::client;

        void Asyncfoo31(const ::bond::bonded<Payload>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo31, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo31(const ::bond::bonded<Payload>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo31, std::move(context), request);
        }
        void Asyncfoo31(const Payload& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            Asyncfoo31(::bond::bonded<Payload>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo31(const Payload& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return Asyncfoo31(::bond::bonded<Payload>{request}, ::std::move(context));
        }

        void Asyncfoo32(const ::std::function<void(::bond::ext::grpc::unary_call_result<Payload>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo32, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<Payload>> Asyncfoo32(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<Payload>(_mfoo32, std::move(context));
        }

        void Asyncfoo33(const ::bond::bonded<Payload>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<Payload>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo33, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<Payload>> Asyncfoo33(const ::bond::bonded<Payload>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<Payload>(_mfoo33, std::move(context), request);
        }
        void Asyncfoo33(const Payload& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<Payload>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            Asyncfoo33(::bond::bonded<Payload>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::grpc::unary_call_result<Payload>> Asyncfoo33(const Payload& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return Asyncfoo33(::bond::bonded<Payload>{request}, ::std::move(context));
        }

        void AsyncConsumesGeneric1(const ::bond::bonded< ::tests::SomeBox<int32_t>>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mConsumesGeneric1, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<void>> AsyncConsumesGeneric1(const ::bond::bonded< ::tests::SomeBox<int32_t>>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mConsumesGeneric1, std::move(context), request);
        }
        void AsyncConsumesGeneric1(const ::tests::SomeBox<int32_t>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            AsyncConsumesGeneric1(::bond::bonded< ::tests::SomeBox<int32_t>>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> AsyncConsumesGeneric1(const ::tests::SomeBox<int32_t>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return AsyncConsumesGeneric1(::bond::bonded< ::tests::SomeBox<int32_t>>{request}, ::std::move(context));
        }

        void AsyncConsumesGeneric2(const ::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mConsumesGeneric2, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<void>> AsyncConsumesGeneric2(const ::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mConsumesGeneric2, std::move(context), request);
        }
        void AsyncConsumesGeneric2(const ::tests::SomeBox<std::vector<int32_t> >& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            AsyncConsumesGeneric2(::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> AsyncConsumesGeneric2(const ::tests::SomeBox<std::vector<int32_t> >& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return AsyncConsumesGeneric2(::bond::bonded< ::tests::SomeBox<std::vector<int32_t> >>{request}, ::std::move(context));
        }

    private:
        const ::bond::ext::grpc::detail::client::Method _mfoo31{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo31") };
        const ::bond::ext::grpc::detail::client::Method _mfoo32{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo32") };
        const ::bond::ext::grpc::detail::client::Method _mfoo33{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo33") };
        const ::bond::ext::grpc::detail::client::Method _mConsumesGeneric1{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/ConsumesGeneric1") };
        const ::bond::ext::grpc::detail::client::Method _mConsumesGeneric2{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/ConsumesGeneric2") };
    };

    class Service : public ::bond::ext::grpc::detail::service
    {
    public:
        explicit Service(const ::bond::ext::grpc::Scheduler& scheduler)
            : ::bond::ext::grpc::detail::service(
                scheduler,
                {
                    "/tests.Foo/foo31",
                    "/tests.Foo/foo32",
                    "/tests.Foo/foo33",
                    "/tests.Foo/ConsumesGeneric1",
                    "/tests.Foo/ConsumesGeneric2"
                })
        {}

        virtual void foo31(::bond::ext::grpc::unary_call<Payload, void>) = 0;
        virtual void foo32(::bond::ext::grpc::unary_call<void, Payload>) = 0;
        virtual void foo33(::bond::ext::grpc::unary_call<Payload, Payload>) = 0;
        virtual void ConsumesGeneric1(::bond::ext::grpc::unary_call<::tests::SomeBox<int32_t>, void>) = 0;
        virtual void ConsumesGeneric2(::bond::ext::grpc::unary_call<::tests::SomeBox<std::vector<int32_t> >, void>) = 0;

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
            ::bond::ext::grpc::detail::service::Method<typename Schema::service::foo31> _m0{ _s, 0, ::std::bind(&Service::foo31, &_s, ::std::placeholders::_1) };
            ::bond::ext::grpc::detail::service::Method<typename Schema::service::foo32> _m1{ _s, 1, ::std::bind(&Service::foo32, &_s, ::std::placeholders::_1) };
            ::bond::ext::grpc::detail::service::Method<typename Schema::service::foo33> _m2{ _s, 2, ::std::bind(&Service::foo33, &_s, ::std::placeholders::_1) };
            ::bond::ext::grpc::detail::service::Method<typename Schema::service::ConsumesGeneric1> _m3{ _s, 3, ::std::bind(&Service::ConsumesGeneric1, &_s, ::std::placeholders::_1) };
            ::bond::ext::grpc::detail::service::Method<typename Schema::service::ConsumesGeneric2> _m4{ _s, 4, ::std::bind(&Service::ConsumesGeneric2, &_s, ::std::placeholders::_1) };
        };

        ::boost::optional<data> _data;
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

