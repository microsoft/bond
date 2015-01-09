#include "record_streaming_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::record_streaming;


bond::blob SerializeStream()
{
    // bond::OutputBuffer implements Bond output stream interface on top of
    // a series of memory buffers.
    bond::OutputBuffer buffer;
    
    // Use Compact Binary protocol for encoding; the data will be written to
    // the OutputBuffer which will allocate memory as needed.
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    for (unsigned n = 1; n < 10; ++n)
    {
        Struct obj;

        obj.n = n;
        obj.str = std::string(n, '#');

        // Serialize the object
        bond::Serialize(obj, writer);
    }

    // At this point the OutputBuffer contains serialized representation of a
    // stream of objects. Depending on the size, serialized data may be spread
    // across one or more memory blocks. Application can get all these blocks 
    // using GetBuffers method, or merge them into a single block using 
    // GetBuffer method.
    return buffer.GetBuffer();
}


void DeserializeStream(const bond::blob& blob)
{
    typedef bond::CompactBinaryReader<bond::InputBuffer> Reader;
    
    bond::InputBuffer   buffer(blob);
    Reader              reader(buffer);
    
    // Note that we are instantiating bond::bonded class with Reader reference.
    // If we defined bonded object as bond::bonded<Struct>, each call to
    // Deserialize would starts from the beginning of the buffer. Since we want
    // to Deserialize a stream of objects, we pass reader by reference.
    bond::bonded<Struct, Reader&> bonded(reader);
    
    while (!reader.GetBuffer().IsEof())
    {
        Struct obj;

        bonded.Deserialize(obj);
    }
}


int main()
{
    // bond::blob represents memory buffer; it is a lightweight object and can
    // be copied by value
    bond::blob output;
    
    output = SerializeStream();

    // Access the raw memory buffer containing serialization results
    const void* buffer = output.data();
    uint32_t    size = output.size();
    
    // Construct a blob object from a raw memory buffer
    bond::blob input(buffer, size);

    DeserializeStream(input);
    
    return 0;    
}
