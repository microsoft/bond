
#include "bond_meta_reflection.h"
#include <bond/core/exception.h>

namespace deprecated
{
namespace bondmeta
{
    
    const ::bond::Metadata HasMetaFields::Schema::metadata
        = HasMetaFields::Schema::GetMetadata();
    
    const ::bond::Metadata HasMetaFields::Schema::s_full_name_metadata
        = ::bond::reflection::MetadataInit("full_name");
    
    const ::bond::Metadata HasMetaFields::Schema::s_name_metadata
        = ::bond::reflection::MetadataInit("name");

    
} // namespace bondmeta
} // namespace deprecated

namespace {
    // this is a dummy definition to make sure that this compilation unit is never empty
    extern bool empty = false;
}
