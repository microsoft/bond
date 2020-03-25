
#pragma once

#include "custom_alias_with_allocator_types.h"
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
                my::list<bool, arena>,
                &foo::l,
                &s_l_metadata
            > {};

            struct l l;
        
            // v
            struct v : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                foo,
                my::vector<bool, arena>,
                &foo::v,
                &s_v_metadata
            > {};

            struct v v;
        
            // s
            struct s : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                foo,
                my::set<bool, arena>,
                &foo::s,
                &s_s_metadata
            > {};

            struct s s;
        
            // m
            struct m : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                foo,
                my::map<my::string<arena>, bool, arena>,
                &foo::m,
                &s_m_metadata
            > {};

            struct m m;
        
            // st
            struct st : ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                foo,
                my::string<arena>,
                &foo::st,
                &s_st_metadata
            > {};

            struct st st;
        
            // d
            struct d : ::bond::reflection::FieldTemplate<
                5,
                ::bond::reflection::optional_field_modifier,
                foo,
                my::string<arena>,
                &foo::d,
                &s_d_metadata
            > {};

            struct d d;
        
            // l1
            struct l1 : ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<my::list<bool, arena> >,
                &foo::l1,
                &s_l1_metadata
            > {};

            struct l1 l1;
        
            // v1
            struct v1 : ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<my::vector<bool, arena> >,
                &foo::v1,
                &s_v1_metadata
            > {};

            struct v1 v1;
        
            // s1
            struct s1 : ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<my::set<bool, arena> >,
                &foo::s1,
                &s_s1_metadata
            > {};

            struct s1 s1;
        
            // m1
            struct m1 : ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<my::map<my::string<arena>, bool, arena> >,
                &foo::m1,
                &s_m1_metadata
            > {};

            struct m1 m1;
        
            // st1
            struct st1 : ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<my::string<arena> >,
                &foo::st1,
                &s_st1_metadata
            > {};

            struct st1 st1;
        
            // na
            struct na : ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                foo,
                my::set<my::list<my::map<int32_t, my::string<arena>, arena>, arena>, arena>,
                &foo::na,
                &s_na_metadata
            > {};

            struct na na;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::na>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::st1>::type fields2;
        private: typedef boost::mpl::push_front<fields2, var::m1>::type fields3;
        private: typedef boost::mpl::push_front<fields3, var::s1>::type fields4;
        private: typedef boost::mpl::push_front<fields4, var::v1>::type fields5;
        private: typedef boost::mpl::push_front<fields5, var::l1>::type fields6;
        private: typedef boost::mpl::push_front<fields6, var::d>::type fields7;
        private: typedef boost::mpl::push_front<fields7, var::st>::type fields8;
        private: typedef boost::mpl::push_front<fields8, var::m>::type fields9;
        private: typedef boost::mpl::push_front<fields9, var::s>::type fields10;
        private: typedef boost::mpl::push_front<fields10, var::v>::type fields11;
        private: typedef boost::mpl::push_front<fields11, var::l>::type fields12;

        public: typedef fields12::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("foo", "test.foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
