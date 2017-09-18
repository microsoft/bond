
#include "service_attributes_reflection.h"
#include "service_attributes_grpc.h"

namespace tests
{
    
    const ::bond::Metadata Foo::Schema::metadata
        = ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                boost::assign::map_list_of<std::string, std::string>
                    ("FooAttribute", "Bar"));
    
    const ::bond::Metadata Foo::Schema::s_foo_metadata
        = ::bond::reflection::MetadataInit("foo",
                boost::assign::map_list_of<std::string, std::string>
                    ("foo", "method")
                    ("method", ""));

    
} // namespace tests
