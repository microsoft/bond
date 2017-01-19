
#include "service_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata dummy::Schema::metadata
        = dummy::Schema::GetMetadata();
    
    const ::bond::Metadata dummy::Schema::s_count_metadata
        = ::bond::reflection::MetadataInit("count");

    
} // namespace tests

namespace {
    // this is a dummy definition to make sure that this compilation unit is never empty
    extern bool empty = false;
}
