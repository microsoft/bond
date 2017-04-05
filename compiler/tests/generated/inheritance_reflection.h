
#pragma once

#include "inheritance_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // Base
    //
    struct Base::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_x_metadata;

        public: struct var
        {
            // x
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Base,
                int32_t,
                &Base::x,
                &s_x_metadata
            > x;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::x field; };

        public: typedef fields1 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Base", "tests.Base",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    //
    // Foo
    //
    struct Foo::Schema
    {
        typedef ::tests::Base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_x_metadata;

        public: struct var
        {
            // x
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::x,
                &s_x_metadata
            > x;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::x field; };

        public: typedef fields1 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
