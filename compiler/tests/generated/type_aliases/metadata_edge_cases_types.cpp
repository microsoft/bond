
#include "metadata_edge_cases_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const ::bond::Metadata Foo::Schema::s_id_metadata
        = ::bond::reflection::MetadataInit("id");
    
    const ::bond::Metadata Foo::Schema::s_metadata_metadata
        = ::bond::reflection::MetadataInit("metadata");
    
    const ::bond::Metadata Foo::Schema::s__id_metadata
        = ::bond::reflection::MetadataInit("_id");
    
    const ::bond::Metadata Foo::Schema::s_x_metadata
        = ::bond::reflection::MetadataInit("x");
    
    const ::bond::Metadata Foo::Schema::s__x_metadata
        = ::bond::reflection::MetadataInit("_x");

    
} // namespace tests
