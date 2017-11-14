
#include "alias_with_allocator_reflection.h"
#include <bond/core/exception.h>

namespace test
{
    
    const ::bond::Metadata foo::Schema::metadata
        = foo::Schema::GetMetadata();
    
    const ::bond::Metadata foo::Schema::s_l_metadata
        = ::bond::reflection::MetadataInit("l");
    
    const ::bond::Metadata foo::Schema::s_v_metadata
        = ::bond::reflection::MetadataInit("v");
    
    const ::bond::Metadata foo::Schema::s_s_metadata
        = ::bond::reflection::MetadataInit("s");
    
    const ::bond::Metadata foo::Schema::s_m_metadata
        = ::bond::reflection::MetadataInit("m");
    
    const ::bond::Metadata foo::Schema::s_st_metadata
        = ::bond::reflection::MetadataInit("st");
    
    const ::bond::Metadata foo::Schema::s_d_metadata
        = ::bond::reflection::MetadataInit("foo", "d");
    
    const ::bond::Metadata foo::Schema::s_l1_metadata
        = ::bond::reflection::MetadataInit(::bond::nothing, "l1");
    
    const ::bond::Metadata foo::Schema::s_v1_metadata
        = ::bond::reflection::MetadataInit(::bond::nothing, "v1");
    
    const ::bond::Metadata foo::Schema::s_s1_metadata
        = ::bond::reflection::MetadataInit(::bond::nothing, "s1");
    
    const ::bond::Metadata foo::Schema::s_m1_metadata
        = ::bond::reflection::MetadataInit(::bond::nothing, "m1");
    
    const ::bond::Metadata foo::Schema::s_st1_metadata
        = ::bond::reflection::MetadataInit(::bond::nothing, "st1");
    
    const ::bond::Metadata foo::Schema::s_na_metadata
        = ::bond::reflection::MetadataInit("na");

    
    const ::bond::Metadata withFoo::Schema::metadata
        = withFoo::Schema::GetMetadata();
    
    const ::bond::Metadata withFoo::Schema::s_f_metadata
        = ::bond::reflection::MetadataInit("f");
    
    const ::bond::Metadata withFoo::Schema::s_f1_metadata
        = ::bond::reflection::MetadataInit("f1");

    
} // namespace test
