
#pragma once

#include "alias_key_types.h"
#include <bond/core/reflection.h>

namespace test
{
    //
    // foo
    //
    struct foo::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_m_metadata;
        private: static const ::bond::Metadata s_s_metadata;

        public: struct var
        {
            // m
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::map<std::string, int32_t>,
                &foo::m,
                &s_m_metadata
            > m;
        
            // s
            typedef ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::set<int32_t>,
                &foo::s,
                &s_s_metadata
            > s;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::s field; };
        public: struct fields2 { typedef fields1 tail; typedef var::m field; };

        public: typedef fields2 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("foo", "test.foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
