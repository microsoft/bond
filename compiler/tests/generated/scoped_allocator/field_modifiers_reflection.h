
#pragma once

#include "field_modifiers_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // Foo
    //
    struct Foo::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_o_metadata;
        private: static const ::bond::Metadata s_r_metadata;
        private: static const ::bond::Metadata s_ro_metadata;

        public: struct var
        {
            // o
            struct o : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                bool,
                &Foo::o,
                &s_o_metadata
            > {};

            struct o o;
        
            // r
            struct r : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::required_field_modifier,
                Foo,
                int16_t,
                &Foo::r,
                &s_r_metadata
            > {};

            struct r r;
        
            // ro
            struct ro : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::required_optional_field_modifier,
                Foo,
                double,
                &Foo::ro,
                &s_ro_metadata
            > {};

            struct ro ro;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, struct var::ro>::type fields1;
        private: typedef boost::mpl::push_front<fields1, struct var::r>::type fields2;
        private: typedef boost::mpl::push_front<fields2, struct var::o>::type fields3;

        public: typedef fields3::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
