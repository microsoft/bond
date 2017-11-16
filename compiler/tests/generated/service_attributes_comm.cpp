
#include "service_attributes_reflection.h"
#include "service_attributes_comm.h"

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                {
                    { "FooAttribute", "Bar" }
                });
    
    const ::bond::Metadata Foo::Schema::s_foo_metadata
        = ::bond::reflection::MetadataInit("foo",
                {
                    { "foo", "method" },
                    { "method", "" }
                });

    
} // namespace tests
