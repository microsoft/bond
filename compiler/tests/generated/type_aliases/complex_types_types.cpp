
#include "complex_types_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();

    
    const ::bond::Metadata ComplexTypes::Schema::metadata
        = ComplexTypes::Schema::GetMetadata();
    
    const ::bond::Metadata ComplexTypes::Schema::s_li8_metadata
        = ::bond::reflection::MetadataInit("li8");
    
    const ::bond::Metadata ComplexTypes::Schema::s_sb_metadata
        = ::bond::reflection::MetadataInit("sb");
    
    const ::bond::Metadata ComplexTypes::Schema::s_vb_metadata
        = ::bond::reflection::MetadataInit("vb");
    
    const ::bond::Metadata ComplexTypes::Schema::s_nf_metadata
        = ::bond::reflection::MetadataInit("nf");
    
    const ::bond::Metadata ComplexTypes::Schema::s_msws_metadata
        = ::bond::reflection::MetadataInit("msws");
    
    const ::bond::Metadata ComplexTypes::Schema::s_bfoo_metadata
        = ::bond::reflection::MetadataInit("bfoo");
    
    const ::bond::Metadata ComplexTypes::Schema::s_m_metadata
        = ::bond::reflection::MetadataInit("m");

    
} // namespace tests
