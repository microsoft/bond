
#pragma once

#include "service_reflection.h"
#include "service_types.h"
#include "basic_types_grpc.h"
#include "namespace_basic_types_grpc.h"
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
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, void, &s_foo11_metadata> {} foo11;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, void, &s_foo12_metadata> {} foo12;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, void, &s_foo12_impl_metadata> {} foo12_impl;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::BasicTypes, void, &s_foo13_metadata> {} foo13;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::dummy, void, &s_foo14_metadata> {} foo14;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests2::OtherBasicTypes, void, &s_foo15_metadata> {} foo15;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::bond::Void, &s_foo21_metadata> {} foo21;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::bond::Void, &s_foo22_metadata> {} foo22;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::bond::Void, &s_foo23_metadata> {} foo23;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::dummy, ::bond::Void, &s_foo24_metadata> {} foo24;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::tests::BasicTypes, &s_foo31_metadata> {} foo31;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::tests::BasicTypes, &s_foo32_metadata> {} foo32;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::tests::BasicTypes, &s_foo33_metadata> {} foo33;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::tests::BasicTypes, &s__rd_foo33_metadata> {} _rd_foo33;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::dummy, ::tests::BasicTypes, &s_foo34_metadata> {} foo34;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::tests::dummy, &s_foo41_metadata> {} foo41;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::tests::dummy, &s_foo42_metadata> {} foo42;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::tests::dummy, &s_foo43_metadata> {} foo43;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::tests::dummy, ::tests::dummy, &s_foo44_metadata> {} foo44;
            typedef struct : ::bond::ext::gRPC::reflection::MethodTemplate<Foo, ::bond::Void, ::tests::BasicTypes, &s_cq_metadata> {} cq;
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

    class Client : public ::bond::ext::gRPC::detail::client
    {
    public:
        using ::bond::ext::gRPC::detail::client::client;

        void Asyncfoo11(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo11, std::move(context), {});
        }

        void Asyncfoo12(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo12, std::move(context), {});
        }

        void Asyncfoo12_impl(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo12_impl, std::move(context), {});
        }

        void Asyncfoo13(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo13, std::move(context), {}, request);
        }
        void Asyncfoo13(const ::tests::BasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo13(::bond::bonded< ::tests::BasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo14(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo14, std::move(context), {}, request);
        }
        void Asyncfoo14(const ::tests::dummy& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo14(::bond::bonded< ::tests::dummy>{request}, ::std::move(context));
        }

        void Asyncfoo15(const ::bond::bonded< ::tests2::OtherBasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo15, std::move(context), {}, request);
        }
        void Asyncfoo15(const ::tests2::OtherBasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo15(::bond::bonded< ::tests2::OtherBasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo21(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo21, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::bond::Void>> Asyncfoo21(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::bond::Void>(_mfoo21, std::move(context));
        }

        void Asyncfoo22(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo22, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::bond::Void>> Asyncfoo22(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::bond::Void>(_mfoo22, std::move(context));
        }

        void Asyncfoo23(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo23, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::bond::Void>> Asyncfoo23(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::bond::Void>(_mfoo23, std::move(context), request);
        }
        void Asyncfoo23(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo23(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::bond::Void>> Asyncfoo23(const ::tests::BasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo23(::bond::bonded< ::tests::BasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo24(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo24, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::bond::Void>> Asyncfoo24(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::bond::Void>(_mfoo24, std::move(context), request);
        }
        void Asyncfoo24(const ::tests::dummy& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::bond::Void>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo24(::bond::bonded< ::tests::dummy>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::bond::Void>> Asyncfoo24(const ::tests::dummy& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo24(::bond::bonded< ::tests::dummy>{request}, ::std::move(context));
        }

        void Asyncfoo31(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo31, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asyncfoo31(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::BasicTypes>(_mfoo31, std::move(context));
        }

        void Asyncfoo32(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo32, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asyncfoo32(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::BasicTypes>(_mfoo32, std::move(context));
        }

        void Asyncfoo33(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo33, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asyncfoo33(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::BasicTypes>(_mfoo33, std::move(context), request);
        }
        void Asyncfoo33(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo33(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asyncfoo33(const ::tests::BasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo33(::bond::bonded< ::tests::BasicTypes>{request}, ::std::move(context));
        }

        void Async_rd_foo33(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_m_rd_foo33, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Async_rd_foo33(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::BasicTypes>(_m_rd_foo33, std::move(context), request);
        }
        void Async_rd_foo33(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Async_rd_foo33(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Async_rd_foo33(const ::tests::BasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Async_rd_foo33(::bond::bonded< ::tests::BasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo34(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo34, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asyncfoo34(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::BasicTypes>(_mfoo34, std::move(context), request);
        }
        void Asyncfoo34(const ::tests::dummy& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo34(::bond::bonded< ::tests::dummy>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asyncfoo34(const ::tests::dummy& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo34(::bond::bonded< ::tests::dummy>{request}, ::std::move(context));
        }

        void Asyncfoo41(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo41, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::dummy>> Asyncfoo41(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::dummy>(_mfoo41, std::move(context));
        }

        void Asyncfoo42(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo42, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::dummy>> Asyncfoo42(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::dummy>(_mfoo42, std::move(context));
        }

        void Asyncfoo43(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo43, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::tests::dummy>> Asyncfoo43(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::dummy>(_mfoo43, std::move(context), request);
        }
        void Asyncfoo43(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo43(::bond::bonded< ::tests::BasicTypes>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::dummy>> Asyncfoo43(const ::tests::BasicTypes& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo43(::bond::bonded< ::tests::BasicTypes>{request}, ::std::move(context));
        }

        void Asyncfoo44(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mfoo44, std::move(context), cb, request);
        }
        std::future<::bond::ext::gRPC::unary_call_result< ::tests::dummy>> Asyncfoo44(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::dummy>(_mfoo44, std::move(context), request);
        }
        void Asyncfoo44(const ::tests::dummy& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::dummy>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Asyncfoo44(::bond::bonded< ::tests::dummy>{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::dummy>> Asyncfoo44(const ::tests::dummy& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return Asyncfoo44(::bond::bonded< ::tests::dummy>{request}, ::std::move(context));
        }

        void Asynccq(const ::std::function<void(::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_mcq, std::move(context), cb);
        }
        ::std::future<::bond::ext::gRPC::unary_call_result< ::tests::BasicTypes>> Asynccq(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            return ::bond::ext::gRPC::detail::client::dispatch<::tests::BasicTypes>(_mcq, std::move(context));
        }

    private:
        const ::bond::ext::gRPC::detail::client::Method _mfoo11{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo11") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo12{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo12") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo12_impl{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo12_impl") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo13{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo13") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo14{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo14") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo15{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo15") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo21{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo21") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo22{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo22") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo23{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo23") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo24{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo24") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo31{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo31") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo32{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo32") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo33{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo33") };
        const ::bond::ext::gRPC::detail::client::Method _m_rd_foo33{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/_rd_foo33") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo34{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo34") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo41{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo41") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo42{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo42") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo43{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo43") };
        const ::bond::ext::gRPC::detail::client::Method _mfoo44{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/foo44") };
        const ::bond::ext::gRPC::detail::client::Method _mcq{ ::bond::ext::gRPC::detail::client::make_method("/tests.Foo/cq") };
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

        virtual void foo11(::bond::ext::gRPC::unary_call< ::bond::Void, ::bond::Void>) = 0;
        virtual void foo12(::bond::ext::gRPC::unary_call< ::bond::Void, ::bond::Void>) = 0;
        virtual void foo12_impl(::bond::ext::gRPC::unary_call< ::bond::Void, ::bond::Void>) = 0;
        virtual void foo13(::bond::ext::gRPC::unary_call< ::tests::BasicTypes, ::bond::Void>) = 0;
        virtual void foo14(::bond::ext::gRPC::unary_call< ::tests::dummy, ::bond::Void>) = 0;
        virtual void foo15(::bond::ext::gRPC::unary_call< ::tests2::OtherBasicTypes, ::bond::Void>) = 0;
        virtual void foo21(::bond::ext::gRPC::unary_call< ::bond::Void, ::bond::Void>) = 0;
        virtual void foo22(::bond::ext::gRPC::unary_call< ::bond::Void, ::bond::Void>) = 0;
        virtual void foo23(::bond::ext::gRPC::unary_call< ::tests::BasicTypes, ::bond::Void>) = 0;
        virtual void foo24(::bond::ext::gRPC::unary_call< ::tests::dummy, ::bond::Void>) = 0;
        virtual void foo31(::bond::ext::gRPC::unary_call< ::bond::Void, ::tests::BasicTypes>) = 0;
        virtual void foo32(::bond::ext::gRPC::unary_call< ::bond::Void, ::tests::BasicTypes>) = 0;
        virtual void foo33(::bond::ext::gRPC::unary_call< ::tests::BasicTypes, ::tests::BasicTypes>) = 0;
        virtual void _rd_foo33(::bond::ext::gRPC::unary_call< ::tests::BasicTypes, ::tests::BasicTypes>) = 0;
        virtual void foo34(::bond::ext::gRPC::unary_call< ::tests::dummy, ::tests::BasicTypes>) = 0;
        virtual void foo41(::bond::ext::gRPC::unary_call< ::bond::Void, ::tests::dummy>) = 0;
        virtual void foo42(::bond::ext::gRPC::unary_call< ::bond::Void, ::tests::dummy>) = 0;
        virtual void foo43(::bond::ext::gRPC::unary_call< ::tests::BasicTypes, ::tests::dummy>) = 0;
        virtual void foo44(::bond::ext::gRPC::unary_call< ::tests::dummy, ::tests::dummy>) = 0;
        virtual void cq(::bond::ext::gRPC::unary_call< ::bond::Void, ::tests::BasicTypes>) = 0;

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

