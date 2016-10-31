
#include "field_modifiers_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const ::bond::Metadata Foo::Schema::s_o_metadata
        = ::bond::reflection::MetadataInit("o");
    
    const ::bond::Metadata Foo::Schema::s_r_metadata
        = ::bond::reflection::MetadataInit("r", ::bond::reflection::required_field_modifier::value,
                ::bond::reflection::Attributes());
    
    const ::bond::Metadata Foo::Schema::s_ro_metadata
        = ::bond::reflection::MetadataInit("ro", ::bond::reflection::required_optional_field_modifier::value,
                ::bond::reflection::Attributes());

    
} // namespace tests
