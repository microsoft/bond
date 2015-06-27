
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
        typedef bond::no_base base;

        static const bond::Metadata metadata;
        
        private: static const bond::Metadata s_m_metadata;
        private: static const bond::Metadata s_s_metadata;

        public: struct var
        {
            // m
            typedef bond::reflection::FieldTemplate<
                0,
                bond::reflection::optional_field_modifier,
                foo,
                std::map<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>, int32_t, std::less<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other> >, typename arena::rebind<std::pair<const std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>, int32_t> >::other>,
                &foo::m,
                &s_m_metadata
            > m;
        
            // s
            typedef bond::reflection::FieldTemplate<
                1,
                bond::reflection::optional_field_modifier,
                foo,
                std::set<int32_t, std::less<int32_t>, typename arena::rebind<int32_t>::other>,
                &foo::s,
                &s_s_metadata
            > s;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::s>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::m>::type fields2;

        public: typedef fields2::type fields;
        
        
        static bond::Metadata GetMetadata()
        {
            return bond::reflection::MetadataInit("foo", "test.foo",
                bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
