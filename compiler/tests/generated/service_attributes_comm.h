
#pragma once

#include <bond/comm/services.h>
#include "service_attributes_types.h"


namespace tests
{
    class Foo
    {
    public:
        virtual ~Foo() = default;

        virtual void foo(const ::bond::comm::payload< ::tests::Param>& input,
            const std::function<void (const ::bond::comm::message< ::tests::Result>&)>& callback) = 0;

        struct Schema;
        class Proxy;

        template <template <typename> class Promise>
        class Using;
    };

    struct Foo::Schema
    {
        static const ::bond::Metadata metadata;

        private: static const ::bond::Metadata s_foo_metadata;

        public: struct service
        {
            typedef ::bond::reflection::MethodTemplate<
                Foo,
                ::bond::comm::payload< ::tests::Param>,
                ::bond::comm::message< ::tests::Result>,
                &Foo::foo,
                &s_foo_metadata
            > foo;
        };

        private: typedef boost::mpl::list<> methods0;
        private: typedef boost::mpl::push_front<methods0, service::foo>::type methods1;

        public: typedef methods1::type methods;
        
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

        void foo(const ::bond::comm::payload< ::tests::Param>& input,
            const std::function<void (const ::bond::comm::message< ::tests::Result>&)>& callback) override
        {
            _impl->foo(input, callback);
        }

        void foo(const ::tests::Param& input,
            const std::function<void (const ::bond::comm::message< ::tests::Result>&)>& callback)
        {
            _impl->foo(boost::cref(input), callback);
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

            void foo(const ::bond::comm::payload< ::tests::Param>& input,
                const std::function<void (const ::bond::comm::message< ::tests::Result>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo::metadata.name, input, callback);
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
        virtual auto foo(const ::bond::comm::payload< ::tests::Param>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::Result>>>().get_future()) = 0;

        void foo(const ::bond::comm::payload< ::tests::Param>& input,
            const std::function<void (const ::bond::comm::message< ::tests::Result>&)>& callback) override
        {
            when(foo(input), ::bond::comm::Continuation(callback));
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

        using Foo::Proxy::foo;

        auto foo(const ::bond::comm::payload< ::tests::Param>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::Result>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message< ::tests::Result>>>();

            _impl->foo(input,
                [=](const ::bond::comm::message< ::tests::Result>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo(const ::tests::Param& input)
            -> decltype(std::declval< Promise< ::bond::comm::message< ::tests::Result>>>().get_future())
        {
            return foo(::bond::comm::payload< ::tests::Param>(boost::cref(input)));
        }
        
    };
    

} // namespace tests
