
#pragma once

#include "metadata_edge_cases_types.h"
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
        
        private: static const ::bond::Metadata s_id_metadata;
        private: static const ::bond::Metadata s_metadata_metadata;
        private: static const ::bond::Metadata s_id_type_metadata;
        private: static const ::bond::Metadata s_x_metadata;
        private: static const ::bond::Metadata s_x_type_metadata;

        public: struct var
        {
            // id
            typedef struct id_type0 : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::id,
                &s_id_metadata
            > {} id;
        
            // metadata
            typedef struct metadata_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::metadata,
                &s_metadata_metadata
            > {} metadata;
        
            // id_type
            typedef struct id_type_type : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::id_type,
                &s_id_type_metadata
            > {} id_type;
        
            // x
            typedef struct x_type0 : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::x,
                &s_x_metadata
            > {} x;
        
            // x_type
            typedef struct x_type_type : ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::x_type,
                &s_x_type_metadata
            > {} x_type;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::x_type>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::x>::type fields2;
        private: typedef boost::mpl::push_front<fields2, var::id_type>::type fields3;
        private: typedef boost::mpl::push_front<fields3, var::metadata>::type fields4;
        private: typedef boost::mpl::push_front<fields4, var::id>::type fields5;

        public: typedef fields5::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
