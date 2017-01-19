
#include "inheritance_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Base::Schema::metadata
        = Base::Schema::GetMetadata();
    
    const ::bond::Metadata Base::Schema::s_x_metadata
        = ::bond::reflection::MetadataInit("x");

    
    const ::bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const ::bond::Metadata Foo::Schema::s_x_metadata
        = ::bond::reflection::MetadataInit("x");

    
} // namespace tests

namespace {
    // this is a dummy definition to make sure that this compilation unit is never empty
    extern bool empty = false;
}
