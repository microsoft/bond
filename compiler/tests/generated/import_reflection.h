
#pragma once

#include "import_types.h"
#include <bond/core/reflection.h>
#include "dir1/dir2/empty_reflection.h"

namespace import_test
{
    //
    // HasEmpty
    //
    struct HasEmpty::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_e_metadata;

        public: struct var
        {
            // e
            typedef struct e_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                HasEmpty,
                ::empty::Empty,
                &HasEmpty::e,
                &s_e_metadata
            > {} e;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::e>::type fields1;

        public: typedef fields1::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("HasEmpty", "import_test.HasEmpty",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace import_test
