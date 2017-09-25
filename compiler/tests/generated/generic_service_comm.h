
#pragma once

#include <bond/comm/services.h>
#include "generic_service_types.h"


namespace tests
{
    template <typename Payload>
    class Foo
    {
    public:
        virtual ~Foo() = default;

        virtual void foo31(const ::bond::comm::payload<Payload>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        virtual void foo32(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback) = 0;

        virtual void foo33(const ::bond::comm::payload<Payload>& input,
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback) = 0;

        virtual void ConsumesGeneric1(const ::bond::comm::payload< ::tests::SomeBox<int32_t>>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        virtual void ConsumesGeneric2(const ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) = 0;

        struct Schema;
        class Proxy;

        template <template <typename> class Promise>
        class Using;
    };

    template <typename Payload>
    struct Foo<Payload>::Schema
    {
        static const ::bond::Metadata metadata;

        private: static const ::bond::Metadata s_foo31_metadata;
        private: static const ::bond::Metadata s_foo32_metadata;
        private: static const ::bond::Metadata s_foo33_metadata;
        private: static const ::bond::Metadata s_ConsumesGeneric1_metadata;
        private: static const ::bond::Metadata s_ConsumesGeneric2_metadata;

        public: struct service
        {
            typedef ::bond::reflection::MethodTemplate<
                Foo<Payload>,
                ::bond::comm::payload<Payload>,
                ::bond::comm::message<void>,
                &Foo<Payload>::foo31,
                &s_foo31_metadata
            > foo31;

            typedef ::bond::reflection::MethodTemplate<
                Foo<Payload>,
                ::bond::comm::payload<void>,
                ::bond::comm::message<Payload>,
                &Foo<Payload>::foo32,
                &s_foo32_metadata
            > foo32;

            typedef ::bond::reflection::MethodTemplate<
                Foo<Payload>,
                ::bond::comm::payload<Payload>,
                ::bond::comm::message<Payload>,
                &Foo<Payload>::foo33,
                &s_foo33_metadata
            > foo33;

            typedef ::bond::reflection::MethodTemplate<
                Foo<Payload>,
                ::bond::comm::payload< ::tests::SomeBox<int32_t>>,
                ::bond::comm::message<void>,
                &Foo<Payload>::ConsumesGeneric1,
                &s_ConsumesGeneric1_metadata
            > ConsumesGeneric1;

            typedef ::bond::reflection::MethodTemplate<
                Foo<Payload>,
                ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>,
                ::bond::comm::message<void>,
                &Foo<Payload>::ConsumesGeneric2,
                &s_ConsumesGeneric2_metadata
            > ConsumesGeneric2;
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

    template <typename Payload>
    class Foo<Payload>::Proxy
        : public Foo<Payload>
    {
    public:
        template <typename ServiceProxy>
        explicit
        Proxy(const ServiceProxy& proxy,
              const std::string& name = Foo<Payload>::Schema::metadata.qualified_name)
            : _impl(boost::make_shared<__Impl<ServiceProxy>>(proxy, name))
        {}

        explicit
        Proxy(const boost::shared_ptr<Foo<Payload>>& service)
            : _impl(service)
        {}

        Proxy() = default;

        void foo31(const ::bond::comm::payload<Payload>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->foo31(input, callback);
        }

        void foo32(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback) override
        {
            _impl->foo32(input, callback);
        }

        void foo32(
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback)
        {
            _impl->foo32(::bond::comm::payload<void>(), callback);
        }

        void foo33(const ::bond::comm::payload<Payload>& input,
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback) override
        {
            _impl->foo33(input, callback);
        }

        void ConsumesGeneric1(const ::bond::comm::payload< ::tests::SomeBox<int32_t>>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->ConsumesGeneric1(input, callback);
        }

        void ConsumesGeneric1(const ::tests::SomeBox<int32_t>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback)
        {
            _impl->ConsumesGeneric1(boost::cref(input), callback);
        }

        void ConsumesGeneric2(const ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            _impl->ConsumesGeneric2(input, callback);
        }

        void ConsumesGeneric2(const ::tests::SomeBox<std::vector<int32_t> >& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback)
        {
            _impl->ConsumesGeneric2(boost::cref(input), callback);
        }

        template <template <typename> class Promise>
        class Using;

    protected:
        boost::shared_ptr<Foo<Payload>> _impl;

        template <typename ServiceProxy>
        class __Impl
            : public Foo<Payload>
        {
        public:
            __Impl(const ServiceProxy& proxy, const std::string& name)
                : _proxy(proxy),
                  _name(name)
            {}

            virtual ~__Impl() = default;

            void foo31(const ::bond::comm::payload<Payload>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo31::metadata.name, input, callback);
            }

            void foo32(const ::bond::comm::payload<void>& input,
                const std::function<void (const ::bond::comm::message<Payload>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo32::metadata.name, input, callback);
            }

            void foo33(const ::bond::comm::payload<Payload>& input,
                const std::function<void (const ::bond::comm::message<Payload>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::foo33::metadata.name, input, callback);
            }

            void ConsumesGeneric1(const ::bond::comm::payload< ::tests::SomeBox<int32_t>>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::ConsumesGeneric1::metadata.name, input, callback);
            }

            void ConsumesGeneric2(const ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>& input,
                const std::function<void (const ::bond::comm::message<void>&)>& callback) override
            {
                _proxy.Send(_name, Schema::service::ConsumesGeneric2::metadata.name, input, callback);
            }

        private:
            ServiceProxy _proxy;
            const std::string _name;
        };
    };

    template <typename Payload>
    template <template <typename> class Promise>
    class Foo<Payload>::Using
        : public Foo<Payload>
    {
    public:
        virtual auto foo31(const ::bond::comm::payload<Payload>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        virtual auto foo32(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<Payload>>>().get_future()) = 0;

        virtual auto foo33(const ::bond::comm::payload<Payload>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<Payload>>>().get_future()) = 0;

        virtual auto ConsumesGeneric1(const ::bond::comm::payload< ::tests::SomeBox<int32_t>>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        virtual auto ConsumesGeneric2(const ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future()) = 0;

        void foo31(const ::bond::comm::payload<Payload>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(foo31(input), ::bond::comm::Continuation(callback));
        }

        void foo32(const ::bond::comm::payload<void>& input,
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback) override
        {
            when(foo32(input), ::bond::comm::Continuation(callback));
        }

        void foo33(const ::bond::comm::payload<Payload>& input,
            const std::function<void (const ::bond::comm::message<Payload>&)>& callback) override
        {
            when(foo33(input), ::bond::comm::Continuation(callback));
        }

        void ConsumesGeneric1(const ::bond::comm::payload< ::tests::SomeBox<int32_t>>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(ConsumesGeneric1(input), ::bond::comm::Continuation(callback));
        }

        void ConsumesGeneric2(const ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>& input,
            const std::function<void (const ::bond::comm::message<void>&)>& callback) override
        {
            when(ConsumesGeneric2(input), ::bond::comm::Continuation(callback));
        }
    };

    template <typename Payload>
    template <template <typename> class Promise>
    class Foo<Payload>::Proxy::Using
        : public Foo<Payload>::Proxy
    {
    public:
        template <typename ServiceProxy>
        explicit
        Using(const ServiceProxy& proxy,
              const std::string& name = Foo<Payload>::Schema::metadata.qualified_name)
            : Foo<Payload>::Proxy(proxy, name)
        {}

        explicit
        Using(const boost::shared_ptr<Foo<Payload>>& service)
            : Foo<Payload>::Proxy(service)
        {}

        Using() = default;

        using Foo<Payload>::Proxy::foo31;

        auto foo31(const ::bond::comm::payload<Payload>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->foo31(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        using Foo<Payload>::Proxy::foo32;

        auto foo32(const ::bond::comm::payload<void>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<Payload>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<Payload>>>();

            _impl->foo32(input,
                [=](const ::bond::comm::message<Payload>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto foo32()
            -> decltype(std::declval< Promise< ::bond::comm::message<Payload>>>().get_future())
        {
            return foo32(::bond::comm::payload<void>());
        }

        using Foo<Payload>::Proxy::foo33;

        auto foo33(const ::bond::comm::payload<Payload>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<Payload>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<Payload>>>();

            _impl->foo33(input,
                [=](const ::bond::comm::message<Payload>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        using Foo<Payload>::Proxy::ConsumesGeneric1;

        auto ConsumesGeneric1(const ::bond::comm::payload< ::tests::SomeBox<int32_t>>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->ConsumesGeneric1(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto ConsumesGeneric1(const ::tests::SomeBox<int32_t>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            return ConsumesGeneric1(::bond::comm::payload< ::tests::SomeBox<int32_t>>(boost::cref(input)));
        }
        

        using Foo<Payload>::Proxy::ConsumesGeneric2;

        auto ConsumesGeneric2(const ::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            auto promise = boost::make_shared<Promise< ::bond::comm::message<void>>>();

            _impl->ConsumesGeneric2(input,
                [=](const ::bond::comm::message<void>& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }

        auto ConsumesGeneric2(const ::tests::SomeBox<std::vector<int32_t> >& input)
            -> decltype(std::declval< Promise< ::bond::comm::message<void>>>().get_future())
        {
            return ConsumesGeneric2(::bond::comm::payload< ::tests::SomeBox<std::vector<int32_t> >>(boost::cref(input)));
        }
        
    };
    

} // namespace tests
