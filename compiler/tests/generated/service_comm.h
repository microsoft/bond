
#pragma once

#include <bond/comm/services.h>
#include "service_types.h"
#include "basic_types_comm.h"

namespace tests
{
    class Foo
    {
    public:
        virtual ~Foo() = default;

        virtual void foo11(const ::bond::comm::payload<void>& input) = 0;

        virtual void foo12(const ::bond::comm::payload<void>& input) = 0;

        virtual void foo13(const ::bond::comm::payload< ::tests::BasicTypes>& input) = 0;

        virtual void foo14(const ::bond::comm::payload< ::tests::dummy>& input) = 0;

        virtual void foo21(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        virtual void foo22(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        virtual void foo23(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        virtual void foo24(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        virtual void foo31(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) = 0;

        virtual void foo32(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) = 0;

        virtual void foo33(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) = 0;

        virtual void foo34(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) = 0;

        virtual void foo41(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) = 0;

        virtual void foo42(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) = 0;

        virtual void foo43(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) = 0;

        virtual void foo44(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) = 0;

        struct Schema;
        class Proxy;

        template <template <typename> class Promise>
        class Using;
    };

    struct Foo::Schema
    {
        static const ::bond::Metadata metadata;

        private: static const ::bond::Metadata s_foo11_metadata;
        private: static const ::bond::Metadata s_foo12_metadata;
        private: static const ::bond::Metadata s_foo13_metadata;
        private: static const ::bond::Metadata s_foo14_metadata;
        private: static const ::bond::Metadata s_foo21_metadata;
        private: static const ::bond::Metadata s_foo22_metadata;
        private: static const ::bond::Metadata s_foo23_metadata;
        private: static const ::bond::Metadata s_foo24_metadata;
        private: static const ::bond::Metadata s_foo31_metadata;
        private: static const ::bond::Metadata s_foo32_metadata;
        private: static const ::bond::Metadata s_foo33_metadata;
        private: static const ::bond::Metadata s_foo34_metadata;
        private: static const ::bond::Metadata s_foo41_metadata;
        private: static const ::bond::Metadata s_foo42_metadata;
        private: static const ::bond::Metadata s_foo43_metadata;
        private: static const ::bond::Metadata s_foo44_metadata;

        public: struct service
        {
            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                void,
                &Foo::foo11,
                &s_foo11_metadata
            > foo11;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                void,
                &Foo::foo12,
                &s_foo12_metadata
            > foo12;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::BasicTypes>,
                void,
                &Foo::foo13,
                &s_foo13_metadata
            > foo13;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::dummy>,
                void,
                &Foo::foo14,
                &s_foo14_metadata
            > foo14;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                ::bond::comm::message<void>,
                &Foo::foo21,
                &s_foo21_metadata
            > foo21;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                ::bond::comm::message<void>,
                &Foo::foo22,
                &s_foo22_metadata
            > foo22;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::BasicTypes>,
                ::bond::comm::message<void>,
                &Foo::foo23,
                &s_foo23_metadata
            > foo23;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::dummy>,
                ::bond::comm::message<void>,
                &Foo::foo24,
                &s_foo24_metadata
            > foo24;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                ::bond::comm::message< ::tests::BasicTypes>,
                &Foo::foo31,
                &s_foo31_metadata
            > foo31;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                ::bond::comm::message< ::tests::BasicTypes>,
                &Foo::foo32,
                &s_foo32_metadata
            > foo32;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::BasicTypes>,
                ::bond::comm::message< ::tests::BasicTypes>,
                &Foo::foo33,
                &s_foo33_metadata
            > foo33;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::dummy>,
                ::bond::comm::message< ::tests::BasicTypes>,
                &Foo::foo34,
                &s_foo34_metadata
            > foo34;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                ::bond::comm::message< ::tests::dummy>,
                &Foo::foo41,
                &s_foo41_metadata
            > foo41;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload<void>,
                ::bond::comm::message< ::tests::dummy>,
                &Foo::foo42,
                &s_foo42_metadata
            > foo42;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::BasicTypes>,
                ::bond::comm::message< ::tests::dummy>,
                &Foo::foo43,
                &s_foo43_metadata
            > foo43;

            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::dummy>,
                ::bond::comm::message< ::tests::dummy>,
                &Foo::foo44,
                &s_foo44_metadata
            > foo44;
        };

        private: typedef boost::mpl::list<> methods0;
        private: typedef boost::mpl::push_front<methods0, service::foo44>::type methods1;
        private: typedef boost::mpl::push_front<methods1, service::foo43>::type methods2;
        private: typedef boost::mpl::push_front<methods2, service::foo42>::type methods3;
        private: typedef boost::mpl::push_front<methods3, service::foo41>::type methods4;
        private: typedef boost::mpl::push_front<methods4, service::foo34>::type methods5;
        private: typedef boost::mpl::push_front<methods5, service::foo33>::type methods6;
        private: typedef boost::mpl::push_front<methods6, service::foo32>::type methods7;
        private: typedef boost::mpl::push_front<methods7, service::foo31>::type methods8;
        private: typedef boost::mpl::push_front<methods8, service::foo24>::type methods9;
        private: typedef boost::mpl::push_front<methods9, service::foo23>::type methods10;
        private: typedef boost::mpl::push_front<methods10, service::foo22>::type methods11;
        private: typedef boost::mpl::push_front<methods11, service::foo21>::type methods12;
        private: typedef boost::mpl::push_front<methods12, service::foo14>::type methods13;
        private: typedef boost::mpl::push_front<methods13, service::foo13>::type methods14;
        private: typedef boost::mpl::push_front<methods14, service::foo12>::type methods15;
        private: typedef boost::mpl::push_front<methods15, service::foo11>::type methods16;

        public: typedef methods16::type methods;
        
    };
    

    class Foo::Proxy
        : public Foo
    {
    public:
        template <typename ServiceProxy>
        explicit
        Proxy(const ServiceProxy& proxy,
              const std::string& name = Foo::Schema::metadata.qualified_name)
            : _impl(boost::make_shared<__Impl<ServiceProxy>>(proxy, name))
        {}

        explicit
        Proxy(const boost::shared_ptr<Foo>& service)
            : _impl(service)
        {}

        Proxy() = default;

        void foo11(const ::bond::comm::payload<void>& input) override
        {
            _impl->foo11(input);
        }

        void foo11()
        {
            _impl->foo11(::bond::comm::payload<void>());
        }

        void foo12(const ::bond::comm::payload<void>& input) override
        {
            _impl->foo12(input);
        }

        void foo12()
        {
            _impl->foo12(::bond::comm::payload<void>());
        }

        void foo13(const ::bond::comm::payload< ::tests::BasicTypes>& input) override
        {
            _impl->foo13(input);
        }

        void foo13(const ::tests::BasicTypes& input)
        {
            _impl->foo13(boost::cref(input));
        }

        void foo14(const ::bond::comm::payload< ::tests::dummy>& input) override
        {
            _impl->foo14(input);
        }

        void foo14(const ::tests::dummy& input)
        {
            _impl->foo14(boost::cref(input));
        }

        void foo21(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->foo21(input, callback);
        }

        void foo21(
            const std::function<void (const ::bond::comm::message<void>&)>& callback)
        {
            _impl->foo21(::bond::comm::payload<void>(), callback);
        }

        void foo22(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->foo22(input, callback);
        }

        void foo22(
            const std::function<void (const ::bond::comm::message<void>&)>& callback)
        {
            _impl->foo22(::bond::comm::payload<void>(), callback);
        }

        void foo23(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->foo23(input, callback);
        }

        void foo23(const ::tests::BasicTypes& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback)
        {
            _impl->foo23(boost::cref(input), callback);
        }

        void foo24(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->foo24(input, callback);
        }

        void foo24(const ::tests::dummy& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback)
        {
            _impl->foo24(boost::cref(input), callback);
        }

        void foo31(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            _impl->foo31(input, callback);
        }

        void foo31(
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback)
        {
            _impl->foo31(::bond::comm::payload<void>(), callback);
        }

        void foo32(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            _impl->foo32(input, callback);
        }

        void foo32(
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback)
        {
            _impl->foo32(::bond::comm::payload<void>(), callback);
        }

        void foo33(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            _impl->foo33(input, callback);
        }

        void foo33(const ::tests::BasicTypes& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback)
        {
            _impl->foo33(boost::cref(input), callback);
        }

        void foo34(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            _impl->foo34(input, callback);
        }

        void foo34(const ::tests::dummy& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback)
        {
            _impl->foo34(boost::cref(input), callback);
        }

        void foo41(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            _impl->foo41(input, callback);
        }

        void foo41(
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback)
        {
            _impl->foo41(::bond::comm::payload<void>(), callback);
        }

        void foo42(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            _impl->foo42(input, callback);
        }

        void foo42(
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback)
        {
            _impl->foo42(::bond::comm::payload<void>(), callback);
        }

        void foo43(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            _impl->foo43(input, callback);
        }

        void foo43(const ::tests::BasicTypes& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback)
        {
            _impl->foo43(boost::cref(input), callback);
        }

        void foo44(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            _impl->foo44(input, callback);
        }

        void foo44(const ::tests::dummy& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback)
        {
            _impl->foo44(boost::cref(input), callback);
        }

        template <template <typename> class Promise>
        class Using;

    protected:
        boost::shared_ptr<Foo> _impl;

        template <typename ServiceProxy>
        class __Impl
            : public Foo
        {
        public:
            __Impl(const ServiceProxy& proxy, const std::string& name)
                : _proxy(proxy),
                  _name(name)
            {}

            virtual ~__Impl() = default;

            void foo11(const ::bond::comm::payload<void>& input) override
            {
                _proxy.Send(_name, Schema::service::foo11::metadata.name, input);
            }

            void foo12(const ::bond::comm::payload<void>& input) override
            {
                _proxy.Send(_name, Schema::service::foo12::metadata.name, input);
            }

            void foo13(const ::bond::comm::payload< ::tests::BasicTypes>& input) override
            {
                _proxy.Send(_name, Schema::service::foo13::metadata.name, input);
            }

            void foo14(const ::bond::comm::payload< ::tests::dummy>& input) override
            {
                _proxy.Send(_name, Schema::service::foo14::metadata.name, input);
            }

            void foo21(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo21::metadata.name, input, callback);
            }

            void foo22(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo22::metadata.name, input, callback);
            }

            void foo23(const ::bond::comm::payload< ::tests::BasicTypes>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo23::metadata.name, input, callback);
            }

            void foo24(const ::bond::comm::payload< ::tests::dummy>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo24::metadata.name, input, callback);
            }

            void foo31(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo31::metadata.name, input, callback);
            }

            void foo32(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo32::metadata.name, input, callback);
            }

            void foo33(const ::bond::comm::payload< ::tests::BasicTypes>& input,
                const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo33::metadata.name, input, callback);
            }

            void foo34(const ::bond::comm::payload< ::tests::dummy>& input,
                const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo34::metadata.name, input, callback);
            }

            void foo41(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo41::metadata.name, input, callback);
            }

            void foo42(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo42::metadata.name, input, callback);
            }

            void foo43(const ::bond::comm::payload< ::tests::BasicTypes>& input,
                const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo43::metadata.name, input, callback);
            }

            void foo44(const ::bond::comm::payload< ::tests::dummy>& input,
                const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo44::metadata.name, input, callback);
            }

        private:
            ServiceProxy _proxy;
            const std::string _name;
        };
    };

    template <template <typename> class Promise>
    class Foo::Using
        : public Foo
    {
    public:
        virtual auto foo21(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        virtual auto foo22(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        virtual auto foo23(const ::bond::comm::payload< ::tests::BasicTypes>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        virtual auto foo24(const ::bond::comm::payload< ::tests::dummy>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        virtual auto foo31(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future()) = 0;

        virtual auto foo32(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future()) = 0;

        virtual auto foo33(const ::bond::comm::payload< ::tests::BasicTypes>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future()) = 0;

        virtual auto foo34(const ::bond::comm::payload< ::tests::dummy>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future()) = 0;

        virtual auto foo41(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future()) = 0;

        virtual auto foo42(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future()) = 0;

        virtual auto foo43(const ::bond::comm::payload< ::tests::BasicTypes>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future()) = 0;

        virtual auto foo44(const ::bond::comm::payload< ::tests::dummy>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future()) = 0;

        void foo21(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(foo21(input), ::bond::comm::Continuation(callback));
        }

        void foo22(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(foo22(input), ::bond::comm::Continuation(callback));
        }

        void foo23(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(foo23(input), ::bond::comm::Continuation(callback));
        }

        void foo24(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(foo24(input), ::bond::comm::Continuation(callback));
        }

        void foo31(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            when(foo31(input), ::bond::comm::Continuation(callback));
        }

        void foo32(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            when(foo32(input), ::bond::comm::Continuation(callback));
        }

        void foo33(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            when(foo33(input), ::bond::comm::Continuation(callback));
        }

        void foo34(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message< ::tests::BasicTypes>&)>& callback) override
        {
            when(foo34(input), ::bond::comm::Continuation(callback));
        }

        void foo41(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            when(foo41(input), ::bond::comm::Continuation(callback));
        }

        void foo42(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            when(foo42(input), ::bond::comm::Continuation(callback));
        }

        void foo43(const ::bond::comm::payload< ::tests::BasicTypes>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            when(foo43(input), ::bond::comm::Continuation(callback));
        }

        void foo44(const ::bond::comm::payload< ::tests::dummy>& input,
            const std::function<void (const ::bond::comm::message< ::tests::dummy>&)>& callback) override
        {
            when(foo44(input), ::bond::comm::Continuation(callback));
        }
    };

    template <template <typename> class Promise>
    class Foo::Proxy::Using
        : public Foo::Proxy
    {
    public:
        template <typename ServiceProxy>
        explicit
        Using(const ServiceProxy& proxy,
              const std::string& name = Foo::Schema::metadata.qualified_name)
            : Foo::Proxy(proxy, name)
        {}

        explicit
        Using(const boost::shared_ptr<Foo>& service)
            : Foo::Proxy(service)
        {}

        Using() = default;

        using Foo::Proxy::foo21;

        auto foo21(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->foo21(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo21()
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            return foo21(::bond::comm::payload<void>());
        }

        using Foo::Proxy::foo22;

        auto foo22(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->foo22(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo22()
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            return foo22(::bond::comm::payload<void>());
        }

        using Foo::Proxy::foo23;

        auto foo23(const ::bond::comm::payload< ::tests::BasicTypes>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->foo23(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo23(const ::tests::BasicTypes& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            return foo23(::bond::comm::payload< ::tests::BasicTypes>(boost::cref(input)));
        }
        

        using Foo::Proxy::foo24;

        auto foo24(const ::bond::comm::payload< ::tests::dummy>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->foo24(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo24(const ::tests::dummy& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            return foo24(::bond::comm::payload< ::tests::dummy>(boost::cref(input)));
        }
        

        using Foo::Proxy::foo31;

        auto foo31(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::BasicTypes>>>();

            _impl->foo31(input,
                [=](const ::bond::comm::message< ::tests::BasicTypes>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo31()
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            return foo31(::bond::comm::payload<void>());
        }

        using Foo::Proxy::foo32;

        auto foo32(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::BasicTypes>>>();

            _impl->foo32(input,
                [=](const ::bond::comm::message< ::tests::BasicTypes>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo32()
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            return foo32(::bond::comm::payload<void>());
        }

        using Foo::Proxy::foo33;

        auto foo33(const ::bond::comm::payload< ::tests::BasicTypes>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::BasicTypes>>>();

            _impl->foo33(input,
                [=](const ::bond::comm::message< ::tests::BasicTypes>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo33(const ::tests::BasicTypes& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            return foo33(::bond::comm::payload< ::tests::BasicTypes>(boost::cref(input)));
        }
        

        using Foo::Proxy::foo34;

        auto foo34(const ::bond::comm::payload< ::tests::dummy>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::BasicTypes>>>();

            _impl->foo34(input,
                [=](const ::bond::comm::message< ::tests::BasicTypes>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo34(const ::tests::dummy& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::BasicTypes>>>().get_future())
        {
            return foo34(::bond::comm::payload< ::tests::dummy>(boost::cref(input)));
        }
        

        using Foo::Proxy::foo41;

        auto foo41(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::dummy>>>();

            _impl->foo41(input,
                [=](const ::bond::comm::message< ::tests::dummy>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo41()
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            return foo41(::bond::comm::payload<void>());
        }

        using Foo::Proxy::foo42;

        auto foo42(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::dummy>>>();

            _impl->foo42(input,
                [=](const ::bond::comm::message< ::tests::dummy>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo42()
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            return foo42(::bond::comm::payload<void>());
        }

        using Foo::Proxy::foo43;

        auto foo43(const ::bond::comm::payload< ::tests::BasicTypes>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::dummy>>>();

            _impl->foo43(input,
                [=](const ::bond::comm::message< ::tests::dummy>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo43(const ::tests::BasicTypes& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            return foo43(::bond::comm::payload< ::tests::BasicTypes>(boost::cref(input)));
        }
        

        using Foo::Proxy::foo44;

        auto foo44(const ::bond::comm::payload< ::tests::dummy>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::dummy>>>();

            _impl->foo44(input,
                [=](const ::bond::comm::message< ::tests::dummy>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo44(const ::tests::dummy& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::dummy>>>().get_future())
        {
            return foo44(::bond::comm::payload< ::tests::dummy>(boost::cref(input)));
        }
        
    };
    

} // namespace tests
