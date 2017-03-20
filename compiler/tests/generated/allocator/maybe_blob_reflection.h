
#pragma once

#include "maybe_blob_types.h"
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
        
        private: static const ::bond::Metadata s_b_metadata;

        public: struct var
        {
            // b
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe< ::bond::blob>,
                &Foo::b,
                &s_b_metadata
            > b;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::b field; };

        public: typedef fields1 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
