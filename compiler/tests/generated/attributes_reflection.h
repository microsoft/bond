
#pragma once

#include "attributes_types.h"
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
        
        private: static const ::bond::Metadata s_f_metadata;

        public: struct var
        {
            // f
            typedef struct f_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                std::string,
                &Foo::f,
                &s_f_metadata
            > {} f;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::f>::type fields1;

        public: typedef fields1::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                {
                    { "StructAttribute1", "one" },
                    { "StructAttribute2", "two" }
                }
            );
        }
    };
    

    
} // namespace tests
