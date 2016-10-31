
#include "maybe_blob_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const ::bond::Metadata Foo::Schema::s_b_metadata
        = ::bond::reflection::MetadataInit(::bond::nothing, "b");

    
} // namespace tests
