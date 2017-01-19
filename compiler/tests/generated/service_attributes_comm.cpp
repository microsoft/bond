
#include "service_attributes_reflection.h"
#include "service_attributes_comm.h"
#include <bond/core/exception.h>

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

namespace {
    // this is a dummy definition to make sure that this compilation unit is never empty
    extern bool empty = false;
}
