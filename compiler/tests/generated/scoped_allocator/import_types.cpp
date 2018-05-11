
#include "import_reflection.h"
#include <bond/core/exception.h>

namespace import_test
{
    
    const ::bond::Metadata HasEmpty::Schema::metadata
        = HasEmpty::Schema::GetMetadata();
    
    const ::bond::Metadata HasEmpty::Schema::s_e_metadata
        = ::bond::reflection::MetadataInit("e");

    
} // namespace import_test
