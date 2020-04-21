
#pragma once

#include "service_types.h"
#include <bond/core/reflection.h>
#include "basic_types_reflection.h"
#include "namespace_basic_types_reflection.h"

namespace tests
{
    //
    // dummy
    //
    struct dummy::Schema
    {
        typedef ::bond::no_base base;

        DllExport
        static const ::bond::Metadata metadata;
        
        private: DllExport
        static const ::bond::Metadata s_count_metadata;

        public: struct var
        {
            // count
            typedef struct count_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                dummy,
                int32_t,
                &dummy::count,
                &s_count_metadata
            > {} count;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::count>::type fields1;

        public: typedef fields1::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("dummy", "tests.dummy",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
