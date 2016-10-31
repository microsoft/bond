
#include "service_attributes_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Result::Schema::metadata
        = Result::Schema::GetMetadata();

    
    const ::bond::Metadata Param::Schema::metadata
        = Param::Schema::GetMetadata();

    
} // namespace tests
