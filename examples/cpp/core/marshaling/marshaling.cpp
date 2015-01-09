#include "marshaling_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::marshaling;


bond::blob Marshal(const Struct& obj)
{
    // bond::OutputBuffer implements Bond output stream interface on top of
    // a series of memory buffers.
    bond::OutputBuffer buffer;
    
    // Use Compact Binary protocol for encoding; the data will be written to
    // the OutputBuffer which will allocate memory as needed.
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    // Marshal saves protocol version information and serialized the object
    // the output buffer.
    bond::Marshal(obj, writer);

    // At this point the OutputBuffer contains serialized representation of the
    // object.  Depending on the size, serialized data may be spread across one
    // or more memory blocks. Application can get all these blocks using
    // GetBuffers method, or merge them into a single block using GetBuffer
    // method.
    return buffer.GetBuffer();
}


void Unmarshal(const bond::InputBuffer& buffer, Struct& obj)
{
    // Unmarshal reads protocol version information from input stream and uses
    // appropriate protocol reader to deserialize data.
    bond::Unmarshal(buffer, obj);
}


int main()
{
    Struct obj, obj2;

    A a;

    a.x = 12;
    
    obj.n = 0x1000;
    obj.str = "test";
    obj.items.push_back(3.14);
    obj.items.push_back(0);
    obj.a = bond::bonded<A>(a);

    bond::InputBuffer buffer;
    
    buffer = Marshal(obj);

    Unmarshal(buffer, obj2);
    
    return 0;    
}
