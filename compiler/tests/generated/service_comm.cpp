
#include "service_reflection.h"
#include "service_comm.h"
#include <bond/core/exception.h>

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes());
    
    const ::bond::Metadata Foo::Schema::s_foo11_metadata
        = ::bond::reflection::MetadataInit("foo11");
    
    const ::bond::Metadata Foo::Schema::s_foo12_metadata
        = ::bond::reflection::MetadataInit("foo12");
    
    const ::bond::Metadata Foo::Schema::s_foo13_metadata
        = ::bond::reflection::MetadataInit("foo13");
    
    const ::bond::Metadata Foo::Schema::s_foo14_metadata
        = ::bond::reflection::MetadataInit("foo14");
    
    const ::bond::Metadata Foo::Schema::s_foo21_metadata
        = ::bond::reflection::MetadataInit("foo21");
    
    const ::bond::Metadata Foo::Schema::s_foo22_metadata
        = ::bond::reflection::MetadataInit("foo22");
    
    const ::bond::Metadata Foo::Schema::s_foo23_metadata
        = ::bond::reflection::MetadataInit("foo23");
    
    const ::bond::Metadata Foo::Schema::s_foo24_metadata
        = ::bond::reflection::MetadataInit("foo24");
    
    const ::bond::Metadata Foo::Schema::s_foo31_metadata
        = ::bond::reflection::MetadataInit("foo31");
    
    const ::bond::Metadata Foo::Schema::s_foo32_metadata
        = ::bond::reflection::MetadataInit("foo32");
    
    const ::bond::Metadata Foo::Schema::s_foo33_metadata
        = ::bond::reflection::MetadataInit("foo33");
    
    const ::bond::Metadata Foo::Schema::s_foo34_metadata
        = ::bond::reflection::MetadataInit("foo34");
    
    const ::bond::Metadata Foo::Schema::s_foo41_metadata
        = ::bond::reflection::MetadataInit("foo41");
    
    const ::bond::Metadata Foo::Schema::s_foo42_metadata
        = ::bond::reflection::MetadataInit("foo42");
    
    const ::bond::Metadata Foo::Schema::s_foo43_metadata
        = ::bond::reflection::MetadataInit("foo43");
    
    const ::bond::Metadata Foo::Schema::s_foo44_metadata
        = ::bond::reflection::MetadataInit("foo44");

    
} // namespace tests
