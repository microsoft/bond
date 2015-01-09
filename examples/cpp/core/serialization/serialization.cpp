#include "serialization_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::serialization;


int main()
{
    Struct obj, obj2;

    obj.n = 0x1000;
    obj.str = "test";
    obj.items.push_back(3.14);
    obj.items.push_back(0);

    // bond::OutputBuffer implements Bond output stream interface on top of
    // a series of memory buffers.
    bond::OutputBuffer output;
    
    // Use Compact Binary protocol for encoding; the data will be written to
    // the OutputBuffer which will allocate memory as needed.
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    // Serialize the object
    Serialize(obj, writer);

    // At this point the OutputBuffer contains serialized representation of the
    // object.  Depending on the size, serialized data may be spread across one
    // or more memory blocks. Application can get all these blocks using
    // GetBuffers method, or merge them into a single block using GetBuffer
    // method.
    bond::blob data = output.GetBuffer();

    // Use Compact Binary protocol for decoding (the same protocol that was used
    // for encoding) and InputBuffer which implements input stream interface on
    // top of a memory blob.
    bond::CompactBinaryReader<bond::InputBuffer> reader(data);
    
    // De-serialize the object
    Deserialize(reader, obj2);

    return 0;    
}
