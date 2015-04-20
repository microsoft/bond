
#pragma once

#include "test_types.h"
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
        
        private: static const bond::Metadata s_a_metadata;
        private: static const bond::Metadata s_b_metadata;
        private: static const bond::Metadata s_c_metadata;
        private: static const bond::Metadata s_m_metadata;
        private: static const bond::Metadata s_n_metadata;
        private: static const bond::Metadata s_l_metadata;

        public: struct var
        {
            // a
            typedef bond::reflection::FieldTemplate<
                0,
                bond::reflection::optional_field_modifier,
                foo,
                std::string,
                &foo::a,
                &s_a_metadata
            > a;
        
            // b
            typedef bond::reflection::FieldTemplate<
                1,
                bond::reflection::optional_field_modifier,
                foo,
                int16_t,
                &foo::b,
                &s_b_metadata
            > b;
        
            // c
            typedef bond::reflection::FieldTemplate<
                2,
                bond::reflection::optional_field_modifier,
                foo,
                int16_t,
                &foo::c,
                &s_c_metadata
            > c;
        
            // m
            typedef bond::reflection::FieldTemplate<
                3,
                bond::reflection::optional_field_modifier,
                foo,
                std::map<std::string, bool>,
                &foo::m,
                &s_m_metadata
            > m;
        
            // n
            typedef bond::reflection::FieldTemplate<
                4,
                bond::reflection::optional_field_modifier,
                foo,
                std::list<bond::nullable< ::test::bar> >,
                &foo::n,
                &s_n_metadata
            > n;
        
            // l
            typedef bond::reflection::FieldTemplate<
                5,
                bond::reflection::optional_field_modifier,
                foo,
                std::list<std::set<float> >,
                &foo::l,
                &s_l_metadata
            > l;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::l>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::n>::type fields2;
        private: typedef boost::mpl::push_front<fields2, var::m>::type fields3;
        private: typedef boost::mpl::push_front<fields3, var::c>::type fields4;
        private: typedef boost::mpl::push_front<fields4, var::b>::type fields5;
        private: typedef boost::mpl::push_front<fields5, var::a>::type fields6;

        public: typedef fields6::type fields;
        
        
        static bond::Metadata GetMetadata()
        {
            return bond::reflection::MetadataInit("foo", "test.foo",
                bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
