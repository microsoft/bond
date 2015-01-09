#include "protocol_transcoding_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::protocol_transcoding;


template <typename T>
bond::blob Serialize(const T& obj)
{
    // bond::OutputBuffer implements Bond output stream interface on top of
    // a series of memory buffers.
    bond::OutputBuffer buffer;
    
    // Use Compact Binary protocol for encoding; the data will be written to
    // the OutputBuffer which will allocate memory as needed.
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    // Serialize the object
    bond::Serialize(obj, writer);

    // At this point the OutputBuffer contains serialized representation of the
    // object.  Depending on the size, serialized data may be spread across one
    // or more memory blocks. Application can get all these blocks using
    // GetBuffers method, or merge them into a single block using GetBuffer
    // method.
    return buffer.GetBuffer();
}


void MakeInput(bond::blob& data, bond::blob& schema)
{
    Struct obj;

    obj.n = 0x1000;
    obj.str = "test";
    obj.items.push_back(3.14);
    obj.items.push_back(0);
    obj.users[1].name = "User Name";
    obj.users[1].address = "Redmond";

    data = Serialize(obj);
    schema = Serialize(bond::GetRuntimeSchema<Struct>());
}


int main()
{
    // In order to make the example self contained, we generate blobs with  
    // serialized data and runtime schema. Normally these would come from
    // outside, e.g. from a file or over network.
    bond::blob data_buffer, schema_buffer;
    MakeInput(data_buffer, schema_buffer);

    // De-serialize the schema.
    // We use shared_ptr to allow bonded<T> objects to hold a reference to the
    // schema. In this simple example a local SchemaDef on the stack would work 
    // as well, but it is a good practice to use a shared_ptr, especially when
    // de-serializing to a structure that has any bonded<T> fields. 
    boost::shared_ptr<bond::SchemaDef> schema(boost::make_shared<bond::SchemaDef>());
    {
        bond::CompactBinaryReader<bond::InputBuffer> reader(schema_buffer);
        bond::Deserialize(reader, *schema);
    }
    
    // Create instance of Compact Binary reader from the blob of serialized
    // data. The application must know what protocol was used for serialization.
    bond::CompactBinaryReader<bond::InputBuffer> reader(data_buffer);
    
    // bond::bonded<T> binds the protocol reader with the schema of the object 
    // contained within the serialized data. The schema may be specified at
    // compile-time ...
    bond::bonded<Struct> compile_time_bonded(reader);

    // ... or at runtime.
    bond::bonded<void> runtime_bonded(reader, schema);

    // We transcode the data into a different protocol by serializing the bonded
    // object using appropriate protocol writer.
    
    // Transcode to Simple Binary protocol
    bond::OutputBuffer                              simple_buffer;
    bond::SimpleBinaryWriter<bond::OutputBuffer>    simple_writer(simple_buffer);

    compile_time_bonded.Serialize(simple_writer);
    
    bond::blob simple_output = simple_buffer.GetBuffer();

    // Transcode to Fast Binary protocol
    bond::OutputBuffer                              fast_buffer;
    bond::FastBinaryWriter<bond::OutputBuffer>      fast_writer(fast_buffer);

    runtime_bonded.Serialize(fast_writer);

    bond::blob fast_output = fast_buffer.GetBuffer();

    return 0;    
}
