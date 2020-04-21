
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
            typedef struct l_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::list<bool, typename std::allocator_traits<arena>::template rebind_alloc<bool> >,
                &foo::l,
                &s_l_metadata
            > {} l;
        
            // v
            typedef struct v_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::vector<bool, typename std::allocator_traits<arena>::template rebind_alloc<bool> >,
                &foo::v,
                &s_v_metadata
            > {} v;
        
            // s
            typedef struct s_type : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::set<bool, std::less<bool>, typename std::allocator_traits<arena>::template rebind_alloc<bool> >,
                &foo::s,
                &s_s_metadata
            > {} s;
        
            // m
            typedef struct m_type : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::map<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, bool, std::less<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > >, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, bool> > >,
                &foo::m,
                &s_m_metadata
            > {} m;
        
            // st
            typedef struct st_type : ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >,
                &foo::st,
                &s_st_metadata
            > {} st;
        
            // d
            typedef struct d_type : ::bond::reflection::FieldTemplate<
                5,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >,
                &foo::d,
                &s_d_metadata
            > {} d;
        
            // l1
            typedef struct l1_type : ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<std::list<bool, typename std::allocator_traits<arena>::template rebind_alloc<bool> > >,
                &foo::l1,
                &s_l1_metadata
            > {} l1;
        
            // v1
            typedef struct v1_type : ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<std::vector<bool, typename std::allocator_traits<arena>::template rebind_alloc<bool> > >,
                &foo::v1,
                &s_v1_metadata
            > {} v1;
        
            // s1
            typedef struct s1_type : ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<std::set<bool, std::less<bool>, typename std::allocator_traits<arena>::template rebind_alloc<bool> > >,
                &foo::s1,
                &s_s1_metadata
            > {} s1;
        
            // m1
            typedef struct m1_type : ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<std::map<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, bool, std::less<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > >, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, bool> > > >,
                &foo::m1,
                &s_m1_metadata
            > {} m1;
        
            // st1
            typedef struct st1_type : ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                foo,
                ::bond::maybe<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > >,
                &foo::st1,
                &s_st1_metadata
            > {} st1;
        
            // na
            typedef struct na_type : ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                foo,
                std::set<std::list<std::map<int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > > > >, typename std::allocator_traits<arena>::template rebind_alloc<std::map<int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > > > > > >, std::less<std::list<std::map<int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > > > >, typename std::allocator_traits<arena>::template rebind_alloc<std::map<int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > > > > > > >, typename std::allocator_traits<arena>::template rebind_alloc<std::list<std::map<int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > > > >, typename std::allocator_traits<arena>::template rebind_alloc<std::map<int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const int32_t, std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > > > > > > > >,
                &foo::na,
                &s_na_metadata
            > {} na;
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
            typedef struct f_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                withFoo,
                ::test::foo,
                &withFoo::f,
                &s_f_metadata
            > {} f;
        
            // f1
            typedef struct f1_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                withFoo,
                ::test::foo,
                &withFoo::f1,
                &s_f1_metadata
            > {} f1;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::f1>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::f>::type fields2;

        public: typedef fields2::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("withFoo", "test.withFoo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace test
