
#include "service_reflection.h"
#include "service_grpc.h"

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes());
    
    const ::bond::Metadata Foo::Schema::s_foo11_metadata
        = ::bond::reflection::MetadataInit("foo11");
    
    const ::bond::Metadata Foo::Schema::s_foo12_metadata
        = ::bond::reflection::MetadataInit("foo12");
    
    const ::bond::Metadata Foo::Schema::s_foo12_impl_metadata
        = ::bond::reflection::MetadataInit("foo12_impl");
    
    const ::bond::Metadata Foo::Schema::s_foo13_metadata
        = ::bond::reflection::MetadataInit("foo13");
    
    const ::bond::Metadata Foo::Schema::s_foo14_metadata
        = ::bond::reflection::MetadataInit("foo14");
    
    const ::bond::Metadata Foo::Schema::s_foo15_metadata
        = ::bond::reflection::MetadataInit("foo15");
    
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
    
    const ::bond::Metadata Foo::Schema::s__rd_foo33_metadata
        = ::bond::reflection::MetadataInit("_rd_foo33");
    
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
    
    const ::bond::Metadata Foo::Schema::s_cq_metadata
        = ::bond::reflection::MetadataInit("cq");
    
    const ::bond::Metadata Foo::Schema::s_foo11_type_metadata
        = ::bond::reflection::MetadataInit("foo11_type");
    
    const ::bond::Metadata Foo::Schema::s_MethodTemplate_metadata
        = ::bond::reflection::MetadataInit("MethodTemplate");
    
    const ::bond::Metadata Foo::Schema::s_service_type_metadata
        = ::bond::reflection::MetadataInit("service_type");
    
    const ::bond::Metadata Foo::Schema::s_input_type_metadata
        = ::bond::reflection::MetadataInit("input_type");
    
    const ::bond::Metadata Foo::Schema::s_result_type_metadata
        = ::bond::reflection::MetadataInit("result_type");
    
    const ::bond::Metadata Foo::Schema::s_metadata_metadata
        = ::bond::reflection::MetadataInit("metadata");
    
    const ::bond::Metadata Foo::Schema::s_method_metadata
        = ::bond::reflection::MetadataInit("method");

    
} // namespace tests
