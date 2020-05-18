
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
        private: static const ::bond::Metadata s_foo11_type_metadata;
        private: static const ::bond::Metadata s_MethodTemplate_metadata;
        private: static const ::bond::Metadata s_service_type_metadata;
        private: static const ::bond::Metadata s_input_type_metadata;
        private: static const ::bond::Metadata s_result_type_metadata;
        private: static const ::bond::Metadata s_metadata_metadata;
        private: static const ::bond::Metadata s_method_metadata;

        public: struct service
        {
            typedef struct foo11_type0 : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::bond::reflection::nothing, &s_foo11_metadata> {} foo11;
            typedef struct foo12_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::bond::reflection::nothing, &s_foo12_metadata> {} foo12;
            typedef struct foo12_impl_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::bond::reflection::nothing, &s_foo12_impl_metadata> {} foo12_impl;
            typedef struct foo13_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::bond::reflection::nothing, &s_foo13_metadata> {} foo13;
            typedef struct foo14_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::dummy, ::bond::reflection::nothing, &s_foo14_metadata> {} foo14;
            typedef struct foo15_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests2::OtherBasicTypes, ::bond::reflection::nothing, &s_foo15_metadata> {} foo15;
            typedef struct foo21_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_foo21_metadata> {} foo21;
            typedef struct foo22_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_foo22_metadata> {} foo22;
            typedef struct foo23_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::BasicTypes, void, &s_foo23_metadata> {} foo23;
            typedef struct foo24_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::dummy, void, &s_foo24_metadata> {} foo24;
            typedef struct foo31_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::tests::BasicTypes, &s_foo31_metadata> {} foo31;
            typedef struct foo32_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::tests::BasicTypes, &s_foo32_metadata> {} foo32;
            typedef struct foo33_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::tests::BasicTypes, &s_foo33_metadata> {} foo33;
            typedef struct _rd_foo33_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::tests::BasicTypes, &s__rd_foo33_metadata> {} _rd_foo33;
            typedef struct foo34_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::dummy, ::tests::BasicTypes, &s_foo34_metadata> {} foo34;
            typedef struct foo41_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::tests::dummy, &s_foo41_metadata> {} foo41;
            typedef struct foo42_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::tests::dummy, &s_foo42_metadata> {} foo42;
            typedef struct foo43_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::BasicTypes, ::tests::dummy, &s_foo43_metadata> {} foo43;
            typedef struct foo44_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, ::tests::dummy, ::tests::dummy, &s_foo44_metadata> {} foo44;
            typedef struct cq_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, ::tests::BasicTypes, &s_cq_metadata> {} cq;
            typedef struct foo11_type_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_foo11_type_metadata> {} foo11_type;
            typedef struct MethodTemplate_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_MethodTemplate_metadata> {} MethodTemplate;
            typedef struct service_type_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_service_type_metadata> {} service_type;
            typedef struct input_type_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_input_type_metadata> {} input_type;
            typedef struct result_type_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_result_type_metadata> {} result_type;
            typedef struct metadata_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_metadata_metadata> {} metadata;
            typedef struct method_type : ::bond::ext::grpc::reflection::MethodTemplate<Foo, void, void, &s_method_metadata> {} method;
        };

        private: typedef boost::mpl::list<> methods0;
        private: typedef boost::mpl::push_front<methods0, service::method>::type methods1;
        private: typedef boost::mpl::push_front<methods1, service::metadata>::type methods2;
        private: typedef boost::mpl::push_front<methods2, service::result_type>::type methods3;
        private: typedef boost::mpl::push_front<methods3, service::input_type>::type methods4;
        private: typedef boost::mpl::push_front<methods4, service::service_type>::type methods5;
        private: typedef boost::mpl::push_front<methods5, service::MethodTemplate>::type methods6;
        private: typedef boost::mpl::push_front<methods6, service::foo11_type>::type methods7;
        private: typedef boost::mpl::push_front<methods7, service::cq>::type methods8;
        private: typedef boost::mpl::push_front<methods8, service::foo44>::type methods9;
        private: typedef boost::mpl::push_front<methods9, service::foo43>::type methods10;
        private: typedef boost::mpl::push_front<methods10, service::foo42>::type methods11;
        private: typedef boost::mpl::push_front<methods11, service::foo41>::type methods12;
        private: typedef boost::mpl::push_front<methods12, service::foo34>::type methods13;
        private: typedef boost::mpl::push_front<methods13, service::_rd_foo33>::type methods14;
        private: typedef boost::mpl::push_front<methods14, service::foo33>::type methods15;
        private: typedef boost::mpl::push_front<methods15, service::foo32>::type methods16;
        private: typedef boost::mpl::push_front<methods16, service::foo31>::type methods17;
        private: typedef boost::mpl::push_front<methods17, service::foo24>::type methods18;
        private: typedef boost::mpl::push_front<methods18, service::foo23>::type methods19;
        private: typedef boost::mpl::push_front<methods19, service::foo22>::type methods20;
        private: typedef boost::mpl::push_front<methods20, service::foo21>::type methods21;
        private: typedef boost::mpl::push_front<methods21, service::foo15>::type methods22;
        private: typedef boost::mpl::push_front<methods22, service::foo14>::type methods23;
        private: typedef boost::mpl::push_front<methods23, service::foo13>::type methods24;
        private: typedef boost::mpl::push_front<methods24, service::foo12_impl>::type methods25;
        private: typedef boost::mpl::push_front<methods25, service::foo12>::type methods26;
        private: typedef boost::mpl::push_front<methods26, service::foo11>::type methods27;

        public: typedef methods27::type methods;

        
    };

    class Client : public ::bond::ext::grpc::detail::client
    {
    public:
        using ::bond::ext::grpc::detail::client::client;

        void Asyncfoo11(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo11, std::move(context), {});
        }

        void Asyncfoo12(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo12, std::move(context), {});
        }

        void Asyncfoo12_impl(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo12_impl, std::move(context), {});
        }

        void Asyncfoo13(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo13, std::move(context), {}, request);
        }
        void Asyncfoo13(const ::tests::BasicTypes& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo13, std::move(context), {}, request);
        }

        void Asyncfoo14(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo14, std::move(context), {}, request);
        }
        void Asyncfoo14(const ::tests::dummy& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo14, std::move(context), {}, request);
        }

        void Asyncfoo15(const ::bond::bonded< ::tests2::OtherBasicTypes>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo15, std::move(context), {}, request);
        }
        void Asyncfoo15(const ::tests2::OtherBasicTypes& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo15, std::move(context), {}, request);
        }

        void Asyncfoo21(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo21, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo21(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo21, std::move(context));
        }

        void Asyncfoo22(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo22, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo22(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo22, std::move(context));
        }

        void Asyncfoo23(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo23, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo23(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo23, std::move(context), request);
        }
        void Asyncfoo23(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo23, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo23(const ::tests::BasicTypes& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo23, std::move(context), request);
        }

        void Asyncfoo24(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo24, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo24(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo24, std::move(context), request);
        }
        void Asyncfoo24(const ::tests::dummy& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo24, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo24(const ::tests::dummy& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo24, std::move(context), request);
        }

        void Asyncfoo31(const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo31, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asyncfoo31(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mfoo31, std::move(context));
        }

        void Asyncfoo32(const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo32, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asyncfoo32(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mfoo32, std::move(context));
        }

        void Asyncfoo33(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo33, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asyncfoo33(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mfoo33, std::move(context), request);
        }
        void Asyncfoo33(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo33, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asyncfoo33(const ::tests::BasicTypes& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mfoo33, std::move(context), request);
        }

        void Async_rd_foo33(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_m_rd_foo33, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Async_rd_foo33(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_m_rd_foo33, std::move(context), request);
        }
        void Async_rd_foo33(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_m_rd_foo33, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Async_rd_foo33(const ::tests::BasicTypes& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_m_rd_foo33, std::move(context), request);
        }

        void Asyncfoo34(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo34, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asyncfoo34(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mfoo34, std::move(context), request);
        }
        void Asyncfoo34(const ::tests::dummy& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo34, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asyncfoo34(const ::tests::dummy& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mfoo34, std::move(context), request);
        }

        void Asyncfoo41(const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::dummy>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo41, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::dummy>> Asyncfoo41(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::dummy>(_mfoo41, std::move(context));
        }

        void Asyncfoo42(const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::dummy>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo42, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::dummy>> Asyncfoo42(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::dummy>(_mfoo42, std::move(context));
        }

        void Asyncfoo43(const ::bond::bonded< ::tests::BasicTypes>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::dummy>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo43, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<::tests::dummy>> Asyncfoo43(const ::bond::bonded< ::tests::BasicTypes>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::dummy>(_mfoo43, std::move(context), request);
        }
        void Asyncfoo43(const ::tests::BasicTypes& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::dummy>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo43, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::dummy>> Asyncfoo43(const ::tests::BasicTypes& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::dummy>(_mfoo43, std::move(context), request);
        }

        void Asyncfoo44(const ::bond::bonded< ::tests::dummy>& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::dummy>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo44, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<::tests::dummy>> Asyncfoo44(const ::bond::bonded< ::tests::dummy>& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::dummy>(_mfoo44, std::move(context), request);
        }
        void Asyncfoo44(const ::tests::dummy& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::dummy>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo44, std::move(context), cb, request);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::dummy>> Asyncfoo44(const ::tests::dummy& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::dummy>(_mfoo44, std::move(context), request);
        }

        void Asynccq(const ::std::function<void(::bond::ext::grpc::unary_call_result<::tests::BasicTypes>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mcq, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<::tests::BasicTypes>> Asynccq(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<::tests::BasicTypes>(_mcq, std::move(context));
        }

        void Asyncfoo11_type(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mfoo11_type, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncfoo11_type(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mfoo11_type, std::move(context));
        }

        void AsyncMethodTemplate(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mMethodTemplate, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> AsyncMethodTemplate(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mMethodTemplate, std::move(context));
        }

        void Asyncservice_type(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mservice_type, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncservice_type(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mservice_type, std::move(context));
        }

        void Asyncinput_type(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_minput_type, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncinput_type(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_minput_type, std::move(context));
        }

        void Asyncresult_type(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mresult_type, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncresult_type(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mresult_type, std::move(context));
        }

        void Asyncmetadata(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mmetadata, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncmetadata(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mmetadata, std::move(context));
        }

        void Asyncmethod(const ::std::function<void(::bond::ext::grpc::unary_call_result<void>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_mmethod, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<void>> Asyncmethod(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<void>(_mmethod, std::move(context));
        }

    private:
        const ::bond::ext::grpc::detail::client::Method _mfoo11{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo11") };
        const ::bond::ext::grpc::detail::client::Method _mfoo12{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo12") };
        const ::bond::ext::grpc::detail::client::Method _mfoo12_impl{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo12_impl") };
        const ::bond::ext::grpc::detail::client::Method _mfoo13{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo13") };
        const ::bond::ext::grpc::detail::client::Method _mfoo14{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo14") };
        const ::bond::ext::grpc::detail::client::Method _mfoo15{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo15") };
        const ::bond::ext::grpc::detail::client::Method _mfoo21{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo21") };
        const ::bond::ext::grpc::detail::client::Method _mfoo22{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo22") };
        const ::bond::ext::grpc::detail::client::Method _mfoo23{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo23") };
        const ::bond::ext::grpc::detail::client::Method _mfoo24{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo24") };
        const ::bond::ext::grpc::detail::client::Method _mfoo31{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo31") };
        const ::bond::ext::grpc::detail::client::Method _mfoo32{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo32") };
        const ::bond::ext::grpc::detail::client::Method _mfoo33{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo33") };
        const ::bond::ext::grpc::detail::client::Method _m_rd_foo33{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/_rd_foo33") };
        const ::bond::ext::grpc::detail::client::Method _mfoo34{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo34") };
        const ::bond::ext::grpc::detail::client::Method _mfoo41{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo41") };
        const ::bond::ext::grpc::detail::client::Method _mfoo42{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo42") };
        const ::bond::ext::grpc::detail::client::Method _mfoo43{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo43") };
        const ::bond::ext::grpc::detail::client::Method _mfoo44{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo44") };
        const ::bond::ext::grpc::detail::client::Method _mcq{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/cq") };
        const ::bond::ext::grpc::detail::client::Method _mfoo11_type{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/foo11_type") };
        const ::bond::ext::grpc::detail::client::Method _mMethodTemplate{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/MethodTemplate") };
        const ::bond::ext::grpc::detail::client::Method _mservice_type{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/service_type") };
        const ::bond::ext::grpc::detail::client::Method _minput_type{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/input_type") };
        const ::bond::ext::grpc::detail::client::Method _mresult_type{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/result_type") };
        const ::bond::ext::grpc::detail::client::Method _mmetadata{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/metadata") };
        const ::bond::ext::grpc::detail::client::Method _mmethod{ ::bond::ext::grpc::detail::client::make_method("/tests.Foo/method") };
    };

    class Service : public ::bond::ext::grpc::detail::service
    {
    public:
        explicit Service(const ::bond::ext::grpc::Scheduler& scheduler)
            : ::bond::ext::grpc::detail::service(
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
                    "/tests.Foo/cq",
                    "/tests.Foo/foo11_type",
                    "/tests.Foo/MethodTemplate",
                    "/tests.Foo/service_type",
                    "/tests.Foo/input_type",
                    "/tests.Foo/result_type",
                    "/tests.Foo/metadata",
                    "/tests.Foo/method"
                })
        {}

        virtual void foo11(::bond::ext::grpc::unary_call<void, ::bond::reflection::nothing>) = 0;
        virtual void foo12(::bond::ext::grpc::unary_call<void, ::bond::reflection::nothing>) = 0;
        virtual void foo12_impl(::bond::ext::grpc::unary_call<void, ::bond::reflection::nothing>) = 0;
        virtual void foo13(::bond::ext::grpc::unary_call<::tests::BasicTypes, ::bond::reflection::nothing>) = 0;
        virtual void foo14(::bond::ext::grpc::unary_call<::tests::dummy, ::bond::reflection::nothing>) = 0;
        virtual void foo15(::bond::ext::grpc::unary_call<::tests2::OtherBasicTypes, ::bond::reflection::nothing>) = 0;
        virtual void foo21(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void foo22(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void foo23(::bond::ext::grpc::unary_call<::tests::BasicTypes, void>) = 0;
        virtual void foo24(::bond::ext::grpc::unary_call<::tests::dummy, void>) = 0;
        virtual void foo31(::bond::ext::grpc::unary_call<void, ::tests::BasicTypes>) = 0;
        virtual void foo32(::bond::ext::grpc::unary_call<void, ::tests::BasicTypes>) = 0;
        virtual void foo33(::bond::ext::grpc::unary_call<::tests::BasicTypes, ::tests::BasicTypes>) = 0;
        virtual void _rd_foo33(::bond::ext::grpc::unary_call<::tests::BasicTypes, ::tests::BasicTypes>) = 0;
        virtual void foo34(::bond::ext::grpc::unary_call<::tests::dummy, ::tests::BasicTypes>) = 0;
        virtual void foo41(::bond::ext::grpc::unary_call<void, ::tests::dummy>) = 0;
        virtual void foo42(::bond::ext::grpc::unary_call<void, ::tests::dummy>) = 0;
        virtual void foo43(::bond::ext::grpc::unary_call<::tests::BasicTypes, ::tests::dummy>) = 0;
        virtual void foo44(::bond::ext::grpc::unary_call<::tests::dummy, ::tests::dummy>) = 0;
        virtual void cq(::bond::ext::grpc::unary_call<void, ::tests::BasicTypes>) = 0;
        virtual void foo11_type(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void MethodTemplate(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void service_type(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void input_type(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void result_type(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void metadata(::bond::ext::grpc::unary_call<void, void>) = 0;
        virtual void method(::bond::ext::grpc::unary_call<void, void>) = 0;

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
            ::bond::ext::grpc::detail::service::Method _m0{ _s, 0, ::bond::ext::grpc::detail::service::make_callback(&Service::foo11, _s) };
            ::bond::ext::grpc::detail::service::Method _m1{ _s, 1, ::bond::ext::grpc::detail::service::make_callback(&Service::foo12, _s) };
            ::bond::ext::grpc::detail::service::Method _m2{ _s, 2, ::bond::ext::grpc::detail::service::make_callback(&Service::foo12_impl, _s) };
            ::bond::ext::grpc::detail::service::Method _m3{ _s, 3, ::bond::ext::grpc::detail::service::make_callback(&Service::foo13, _s) };
            ::bond::ext::grpc::detail::service::Method _m4{ _s, 4, ::bond::ext::grpc::detail::service::make_callback(&Service::foo14, _s) };
            ::bond::ext::grpc::detail::service::Method _m5{ _s, 5, ::bond::ext::grpc::detail::service::make_callback(&Service::foo15, _s) };
            ::bond::ext::grpc::detail::service::Method _m6{ _s, 6, ::bond::ext::grpc::detail::service::make_callback(&Service::foo21, _s) };
            ::bond::ext::grpc::detail::service::Method _m7{ _s, 7, ::bond::ext::grpc::detail::service::make_callback(&Service::foo22, _s) };
            ::bond::ext::grpc::detail::service::Method _m8{ _s, 8, ::bond::ext::grpc::detail::service::make_callback(&Service::foo23, _s) };
            ::bond::ext::grpc::detail::service::Method _m9{ _s, 9, ::bond::ext::grpc::detail::service::make_callback(&Service::foo24, _s) };
            ::bond::ext::grpc::detail::service::Method _m10{ _s, 10, ::bond::ext::grpc::detail::service::make_callback(&Service::foo31, _s) };
            ::bond::ext::grpc::detail::service::Method _m11{ _s, 11, ::bond::ext::grpc::detail::service::make_callback(&Service::foo32, _s) };
            ::bond::ext::grpc::detail::service::Method _m12{ _s, 12, ::bond::ext::grpc::detail::service::make_callback(&Service::foo33, _s) };
            ::bond::ext::grpc::detail::service::Method _m13{ _s, 13, ::bond::ext::grpc::detail::service::make_callback(&Service::_rd_foo33, _s) };
            ::bond::ext::grpc::detail::service::Method _m14{ _s, 14, ::bond::ext::grpc::detail::service::make_callback(&Service::foo34, _s) };
            ::bond::ext::grpc::detail::service::Method _m15{ _s, 15, ::bond::ext::grpc::detail::service::make_callback(&Service::foo41, _s) };
            ::bond::ext::grpc::detail::service::Method _m16{ _s, 16, ::bond::ext::grpc::detail::service::make_callback(&Service::foo42, _s) };
            ::bond::ext::grpc::detail::service::Method _m17{ _s, 17, ::bond::ext::grpc::detail::service::make_callback(&Service::foo43, _s) };
            ::bond::ext::grpc::detail::service::Method _m18{ _s, 18, ::bond::ext::grpc::detail::service::make_callback(&Service::foo44, _s) };
            ::bond::ext::grpc::detail::service::Method _m19{ _s, 19, ::bond::ext::grpc::detail::service::make_callback(&Service::cq, _s) };
            ::bond::ext::grpc::detail::service::Method _m20{ _s, 20, ::bond::ext::grpc::detail::service::make_callback(&Service::foo11_type, _s) };
            ::bond::ext::grpc::detail::service::Method _m21{ _s, 21, ::bond::ext::grpc::detail::service::make_callback(&Service::MethodTemplate, _s) };
            ::bond::ext::grpc::detail::service::Method _m22{ _s, 22, ::bond::ext::grpc::detail::service::make_callback(&Service::service_type, _s) };
            ::bond::ext::grpc::detail::service::Method _m23{ _s, 23, ::bond::ext::grpc::detail::service::make_callback(&Service::input_type, _s) };
            ::bond::ext::grpc::detail::service::Method _m24{ _s, 24, ::bond::ext::grpc::detail::service::make_callback(&Service::result_type, _s) };
            ::bond::ext::grpc::detail::service::Method _m25{ _s, 25, ::bond::ext::grpc::detail::service::make_callback(&Service::metadata, _s) };
            ::bond::ext::grpc::detail::service::Method _m26{ _s, 26, ::bond::ext::grpc::detail::service::make_callback(&Service::method, _s) };
        };

        ::boost::optional<data> _data;
    };
};




} // namespace tests

