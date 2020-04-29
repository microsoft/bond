
#pragma once

#include "complex_types_types.h"
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
        

        public: struct var
        {};

        private: typedef boost::mpl::list<> fields0;
        

        public: typedef fields0::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    //
    // ComplexTypes
    //
    struct ComplexTypes::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_li8_metadata;
        private: static const ::bond::Metadata s_sb_metadata;
        private: static const ::bond::Metadata s_vb_metadata;
        private: static const ::bond::Metadata s_nf_metadata;
        private: static const ::bond::Metadata s_msws_metadata;
        private: static const ::bond::Metadata s_bfoo_metadata;
        private: static const ::bond::Metadata s_m_metadata;

        public: struct var
        {
            // li8
            typedef struct li8_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                std::list<int8_t>,
                &ComplexTypes::li8,
                &s_li8_metadata
            > {} li8;
        
            // sb
            typedef struct sb_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                std::set<bool>,
                &ComplexTypes::sb,
                &s_sb_metadata
            > {} sb;
        
            // vb
            typedef struct vb_type : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                std::vector< ::bond::blob>,
                &ComplexTypes::vb,
                &s_vb_metadata
            > {} vb;
        
            // nf
            typedef struct nf_type : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                ::bond::nullable<float>,
                &ComplexTypes::nf,
                &s_nf_metadata
            > {} nf;
        
            // msws
            typedef struct msws_type : ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                std::map<std::string, std::wstring>,
                &ComplexTypes::msws,
                &s_msws_metadata
            > {} msws;
        
            // bfoo
            typedef struct bfoo_type : ::bond::reflection::FieldTemplate<
                5,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                ::bond::bonded< ::tests::Foo>,
                &ComplexTypes::bfoo,
                &s_bfoo_metadata
            > {} bfoo;
        
            // m
            typedef struct m_type : ::bond::reflection::FieldTemplate<
                6,
                ::bond::reflection::optional_field_modifier,
                ComplexTypes,
                std::map<double, std::list<std::vector< ::bond::nullable< ::bond::bonded< ::tests::Bar> > > > >,
                &ComplexTypes::m,
                &s_m_metadata
            > {} m;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::m>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::bfoo>::type fields2;
        private: typedef boost::mpl::push_front<fields2, var::msws>::type fields3;
        private: typedef boost::mpl::push_front<fields3, var::nf>::type fields4;
        private: typedef boost::mpl::push_front<fields4, var::vb>::type fields5;
        private: typedef boost::mpl::push_front<fields5, var::sb>::type fields6;
        private: typedef boost::mpl::push_front<fields6, var::li8>::type fields7;

        public: typedef fields7::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("ComplexTypes", "tests.ComplexTypes",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
