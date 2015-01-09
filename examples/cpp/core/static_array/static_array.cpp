#include "static_array_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::static_array;

int main()
{
    Example obj, obj2;

    obj.features.fill(13);

    // Serialize the object
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);
    Serialize(obj, writer);

    // De-serialize the object
    bond::CompactBinaryReader<bond::InputBuffer> reader(output.GetBuffer());
    Deserialize(reader, obj2);

    BOOST_ASSERT(obj2 == obj);

    return 0;
}
