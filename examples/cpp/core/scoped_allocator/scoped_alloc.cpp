#include "scoped_alloc_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <cassert>

using namespace examples::scoped_allocator;


int main()
{
    MyAllocator<char> alloc{ 123 };

    Struct obj{ alloc };
    obj.ints.push_back(1);
    obj.strings.strings.emplace_back("test");           // Allocator is automatically propagated.
    obj.strings_vector.emplace_back();                  // Allocator is automatically propagated.
    obj.strings_vector[0].strings.emplace_back("test2");// Allocator is automatically propagated.

    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer{ output };

    Serialize(obj, writer);

    bond::CompactBinaryReader<bond::InputBuffer> reader{ output.GetBuffer() };

    Struct obj2{ alloc };
    Deserialize(reader, obj2);

    assert(obj == obj2);

    return 0;
}
