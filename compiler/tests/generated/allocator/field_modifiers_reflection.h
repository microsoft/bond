
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
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                bool,
                &Foo::o,
                &s_o_metadata
            > o;
        
            // r
            typedef ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::required_field_modifier,
                Foo,
                int16_t,
                &Foo::r,
                &s_r_metadata
            > r;
        
            // ro
            typedef ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::required_optional_field_modifier,
                Foo,
                double,
                &Foo::ro,
                &s_ro_metadata
            > ro;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::ro field; };
        public: struct fields2 { typedef fields1 tail; typedef var::r field; };
        public: struct fields3 { typedef fields2 tail; typedef var::o field; };

        public: typedef fields3 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
