
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
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::required_optional_field_modifier,
                HasMetaFields,
                std::string,
                &HasMetaFields::full_name,
                &s_full_name_metadata
            > full_name;
        
            // name
            typedef ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::required_optional_field_modifier,
                HasMetaFields,
                std::string,
                &HasMetaFields::name,
                &s_name_metadata
            > name;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::name field; };
        public: struct fields2 { typedef fields1 tail; typedef var::full_name field; };

        public: typedef fields2 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("HasMetaFields", "deprecated.bondmeta.HasMetaFields",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace bondmeta
} // namespace deprecated
