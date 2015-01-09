#include "runtime_schema_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::runtime_schema;


bond::blob Serialize(const bond::RuntimeSchema& schema)
{
    // bond::OutputBuffer implements Bond output stream interface on top of
    // a series of memory buffers.
    bond::OutputBuffer buffer;
    
    // Use Compact Binary protocol for encoding; the data will be written to
    // the OutputBuffer which will allocate memory as needed.
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    // Serialize the object
    bond::Serialize(schema, writer);

    // At this point the OutputBuffer contains serialized representation of the
    // object. Depending on the size, serialized data may be spread across one
    // or more memory blocks. Application can get all these blocks using
    // GetBuffers method, or merge them into a single block using GetBuffer
    // method.
    return buffer.GetBuffer();
}


void Deserialize(const bond::blob& buffer, bond::SchemaDef& schema)
{
    // Use Compact Binary protocol for decoding (the same protocol that was used
    // for encoding) and InputBuffer which implements input stream interface on
    // top of a memory buffer.
    bond::CompactBinaryReader<bond::InputBuffer> reader(buffer);
    
    // De-serialize the object
    bond::Deserialize(reader, schema);
}


int main()
{
    // Runtime schema is represented by an object of type bond::RuntimeSchema.
    // bond::GetRuntimeSchema template function returns an instance of runtime 
    // schema for the specified Bond type. The schema object is static and is 
    // created during the first call to this function for a particular type.
    bond::RuntimeSchema schema = bond::GetRuntimeSchema<Struct2>();
    
    // bond::RuntimeSchema contains reference to SchemaDef which the object that
    // describes the schema. SchemaDef is a Bond struct itself, and thus can be 
    // serialized...
    bond::blob buffer;
    buffer = Serialize(schema);

    // ... and de-serialized like any other Bond type.
    bond::SchemaDef schema2;
    Deserialize(buffer, schema2);

    // Application can also directly access schema object, for example to
    // extract custom field attributes
    std::string my_attribute = schema2.structs[0].fields[0].metadata.attributes["MyAttribute"];

    return 0;    
}
