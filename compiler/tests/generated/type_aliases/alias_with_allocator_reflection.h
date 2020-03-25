
#pragma once

#include "alias_with_allocator_types.h"
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
        
        private: static const ::bond::Metadata s_l_metadata;
        private: static const ::bond::Metadata s_v_metadata;
        private: static const ::bond::Metadata s_s_metadata;
        private: static const ::bond::Metadata s_m_metadata;
        private: static const ::bond::Metadata s_st_metadata;
        private: static const ::bond::Metadata s_d_metadata;
        private: static const ::bond::Metadata s_l1_metadata;
        private: static const ::bond::Metadata s_v1_metadata;
        private: static const ::bond::Metadata s_s1_metadata;
        private: static const ::bond::Metadata s_m1_metadata;
        private: static const ::bond::Metadata s_st1_metadata;
        private: static const ::bond::Metadata s_na_metadata;

        public: struct var
        {
            // l
            struct l : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::List<bool>,
                &foo::l,
                &s_l_metadata
            > {};

            struct l l;
        
            // v
            struct v : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::Vector<bool>,
                &foo::v,
                &s_v_metadata
            > {};

            struct v v;
        
            // s
            struct s : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::Set<bool>,
                &foo::s,
                &s_s_metadata
            > {};

            struct s s;
        
            // m
            struct m : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::Map< ::test::String, bool>,
                &foo::m,
                &s_m_metadata
            > {};

            struct m m;
        
            // st
            struct st : ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::String,
                &foo::st,
                &s_st_metadata
            > {};

            struct st st;
        
            // d
            struct d : ::bond::reflection::FieldTemplate<
                5,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::String,
                &foo::d,
                &s_d_metadata
            > {};

            struct d d;
        
            // l1
            struct l1 : ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe< ::test::List<bool> >,
                &foo::l1,
                &s_l1_metadata
            > {};

            struct l1 l1;
        
            // v1
            struct v1 : ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe< ::test::Vector<bool> >,
                &foo::v1,
                &s_v1_metadata
            > {};

            struct v1 v1;
        
            // s1
            struct s1 : ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe< ::test::Set<bool> >,
                &foo::s1,
                &s_s1_metadata
            > {};

            struct s1 s1;
        
            // m1
            struct m1 : ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe< ::test::Map< ::test::String, bool> >,
                &foo::m1,
                &s_m1_metadata
            > {};

            struct m1 m1;
        
            // st1
            struct st1 : ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe< ::test::String>,
                &foo::st1,
                &s_st1_metadata
            > {};

            struct st1 st1;
        
            // na
            struct na : ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::test::NestedAliases,
                &foo::na,
                &s_na_metadata
            > {};

            struct na na;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, struct var::na>::type fields1;
        private: typedef boost::mpl::push_front<fields1, struct var::st1>::type fields2;
        private: typedef boost::mpl::push_front<fields2, struct var::m1>::type fields3;
        private: typedef boost::mpl::push_front<fields3, struct var::s1>::type fields4;
        private: typedef boost::mpl::push_front<fields4, struct var::v1>::type fields5;
        private: typedef boost::mpl::push_front<fields5, struct var::l1>::type fields6;
        private: typedef boost::mpl::push_front<fields6, struct var::d>::type fields7;
        private: typedef boost::mpl::push_front<fields7, struct var::st>::type fields8;
        private: typedef boost::mpl::push_front<fields8, struct var::m>::type fields9;
        private: typedef boost::mpl::push_front<fields9, struct var::s>::type fields10;
        private: typedef boost::mpl::push_front<fields10, struct var::v>::type fields11;
        private: typedef boost::mpl::push_front<fields11, struct var::l>::type fields12;

        public: typedef fields12::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("foo", "test.foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    //
    // withFoo
    //
    struct withFoo::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_f_metadata;
        private: static const ::bond::Metadata s_f1_metadata;

        public: struct var
        {
            // f
            struct f : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                withFoo,
                ::test::TheFoo,
                &withFoo::f,
                &s_f_metadata
            > {};

            struct f f;
        
            // f1
            struct f1 : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                withFoo,
                ::test::foo,
                &withFoo::f1,
                &s_f1_metadata
            > {};

            struct f1 f1;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, struct var::f1>::type fields1;
        private: typedef boost::mpl::push_front<fields1, struct var::f>::type fields2;

        public: typedef fields2::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("withFoo", "test.withFoo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
