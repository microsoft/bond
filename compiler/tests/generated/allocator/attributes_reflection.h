
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
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>,
                &Foo::f,
                &s_f_metadata
            > f;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::f field; };

        public: typedef fields1 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                boost::assign::map_list_of<std::string, std::string>
                    ("StructAttribute1", "one")
                    ("StructAttribute2", "two")
            );
        }
    };
    

    
} // namespace tests
