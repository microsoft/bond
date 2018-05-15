#include "capped_allocator_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

#include <cassert>

using namespace examples::capped_allocator;


Struct PopulateStruct()
{
    // Use capped_allocator with unlimited size in order to populate
    // the struct with values.
    bond::ext::capped_allocator<> alloc{ (std::numeric_limits<std::uint32_t>::max)() };

    // A copy of allocator must be passed to corresponding functions.
    Struct obj{ alloc };
    obj.ints.push_back(1);
    obj.strings.strings.emplace_back("test", alloc);
    obj.strings_vector.emplace_back(alloc);
    obj.strings_vector[0].strings.emplace_back("test2", alloc);

    return obj;
}

bond::blob SerializeStruct(const Struct& obj)
{
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer{ output };

    Serialize(obj, writer);

    return output.GetBuffer();
}

int main()
{
    bond::CompactBinaryReader<bond::InputBuffer> reader{
        SerializeStruct(PopulateStruct()) };

    // Limit the allowed memory allocations to 100 bytes.
    Struct obj{ bond::ext::capped_allocator<>{ 100 } };

    try
    {
        Deserialize(reader, obj);
        assert(false);
    }
    catch (const std::length_error&)
    {
        // Some containers throw std::length_error when checking
        // for allocator's max_size prior to actual allocation.
    }
    catch (const std::bad_alloc&)
    {
        // Deserialization will fail, because it will try to
        // allocate more than 100 bytes.
    }

    return 0;
}
