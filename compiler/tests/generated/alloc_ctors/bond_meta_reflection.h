
#pragma once

#include "bond_meta_types.h"
#include <bond/core/reflection.h>

namespace deprecated
{
namespace bondmeta
{
    //
    // HasMetaFields
    //
    struct HasMetaFields::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_full_name_metadata;
        private: static const ::bond::Metadata s_name_metadata;

        public: struct var
        {
            // full_name
            typedef struct full_name_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::required_optional_field_modifier,
                HasMetaFields,
                std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >,
                &HasMetaFields::full_name,
                &s_full_name_metadata
            > {} full_name;
        
            // name
            typedef struct name_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::required_optional_field_modifier,
                HasMetaFields,
                std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >,
                &HasMetaFields::name,
                &s_name_metadata
            > {} name;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::name>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::full_name>::type fields2;

        public: typedef fields2::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("HasMetaFields", "deprecated.bondmeta.HasMetaFields",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace bondmeta
} // namespace deprecated
