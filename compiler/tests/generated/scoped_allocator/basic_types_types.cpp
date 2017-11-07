
#include "basic_types_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata BasicTypes::Schema::metadata
        = BasicTypes::Schema::GetMetadata();
    
    const ::bond::Metadata BasicTypes::Schema::s__bool_metadata
        = ::bond::reflection::MetadataInit("_bool");
    
    const ::bond::Metadata BasicTypes::Schema::s__str_metadata
        = ::bond::reflection::MetadataInit("_str");
    
    const ::bond::Metadata BasicTypes::Schema::s__wstr_metadata
        = ::bond::reflection::MetadataInit("_wstr");
    
    const ::bond::Metadata BasicTypes::Schema::s__uint64_metadata
        = ::bond::reflection::MetadataInit("_uint64");
    
    const ::bond::Metadata BasicTypes::Schema::s__uint16_metadata
        = ::bond::reflection::MetadataInit("_uint16");
    
    const ::bond::Metadata BasicTypes::Schema::s__uint32_metadata
        = ::bond::reflection::MetadataInit("_uint32");
    
    const ::bond::Metadata BasicTypes::Schema::s__uint8_metadata
        = ::bond::reflection::MetadataInit("_uint8");
    
    const ::bond::Metadata BasicTypes::Schema::s__int8_metadata
        = ::bond::reflection::MetadataInit("_int8");
    
    const ::bond::Metadata BasicTypes::Schema::s__int16_metadata
        = ::bond::reflection::MetadataInit("_int16");
    
    const ::bond::Metadata BasicTypes::Schema::s__int32_metadata
        = ::bond::reflection::MetadataInit("_int32");
    
    const ::bond::Metadata BasicTypes::Schema::s__int64_metadata
        = ::bond::reflection::MetadataInit("_int64");
    
    const ::bond::Metadata BasicTypes::Schema::s__double_metadata
        = ::bond::reflection::MetadataInit("_double");
    
    const ::bond::Metadata BasicTypes::Schema::s__float_metadata
        = ::bond::reflection::MetadataInit("_float");
    
    const ::bond::Metadata BasicTypes::Schema::s__blob_metadata
        = ::bond::reflection::MetadataInit("_blob");

    
} // namespace tests
