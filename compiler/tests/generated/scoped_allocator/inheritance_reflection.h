
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
            typedef struct x_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Base,
                int32_t,
                &Base::x,
                &s_x_metadata
            > {} x;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::x>::type fields1;

        public: typedef fields1::type fields;
        
        
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
            typedef struct x_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::x,
                &s_x_metadata
            > {} x;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::x>::type fields1;

        public: typedef fields1::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
