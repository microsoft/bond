
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
            typedef struct m_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::map<std::string, int32_t>,
                &foo::m,
                &s_m_metadata
            > {} m;
        
            // s
            typedef struct s_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::set<int32_t>,
                &foo::s,
                &s_s_metadata
            > {} s;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::s>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::m>::type fields2;

        public: typedef fields2::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("foo", "test.foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
