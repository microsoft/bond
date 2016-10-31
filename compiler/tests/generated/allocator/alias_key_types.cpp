
#include "alias_key_reflection.h"
#include <bond/core/exception.h>

namespace test
{
    
    const ::bond::Metadata foo::Schema::metadata
        = foo::Schema::GetMetadata();
    
    const ::bond::Metadata foo::Schema::s_m_metadata
        = ::bond::reflection::MetadataInit("m");
    
    const ::bond::Metadata foo::Schema::s_s_metadata
        = ::bond::reflection::MetadataInit("s");

    
} // namespace test
