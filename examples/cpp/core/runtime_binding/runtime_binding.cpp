#include "runtime_binding_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <boost/bind/bind.hpp>

using namespace examples::runtime_binding;


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

    return buffer.GetBuffer();
}


void MakeInput(bond::blob& data, bond::blob& schema)
{
    Struct obj;

    obj.number = 0x1000;
    obj.name = "User Name";
    obj.address = "Redmond";
    obj.age = 100;

    data = Serialize(obj);
    schema = Serialize(bond::GetRuntimeSchema<Struct>());
}


bool ByName(const bond::FieldDef& field, const std::string& name)
{
    return field.metadata.name == name;
}


void Map(const bond::RuntimeSchema& from_schema, std::string from_name,
         const bond::RuntimeSchema& to_schema, std::string to_name, 
         bond::Mappings& mappings)
{
    typedef std::vector<bond::FieldDef>::const_iterator Field;

    // Find the field from_name in from_schema ...
    Field from = find_if(from_schema.GetStruct().fields.begin(), 
                         from_schema.GetStruct().fields.end(),
                         boost::bind(&ByName, boost::placeholders::_1, from_name));
    
    // ... and field to_name in to_schema
    Field to = find_if(to_schema.GetStruct().fields.begin(), 
                       to_schema.GetStruct().fields.end(),
                       boost::bind(&ByName, boost::placeholders::_1, to_name));
    
    // Add mapping (for clarity omits error checking)
    mappings[from->id].path.push_back(to->id);
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
    
    // Create instance of Compact Protocol reader from the blob of serialized
    // data. The application must know what protocol was used for serialization.
    bond::CompactBinaryReader<bond::InputBuffer> reader(data_buffer);
    
    // Bind the reader with runtime schema.
    bond::bonded<void> bonded(reader, schema);

    // Create mappings from fields in the serialized data to fields in MyView 
    // struct:
    //      name -> MyView.some_string
    //      age  -> MyView.some_number
    // This simple example only shows mapping of top level struct fields.
    // However mappings support arbitrary nesting of structs and mapping fields
    // to a different level than they appear at in the source data.
    bond::Mappings mappings;
    
    Map(schema, "name", bond::GetRuntimeSchema<MyView>(), "some_string", mappings);
    Map(schema, "age", bond::GetRuntimeSchema<MyView>(), "some_number", mappings);

    // Map the serialized data onto instance of MyView struct according to
    // specified field mappings. Of course the mappings need to be created only
    // once for a pair of schemas.
    MyView view;
    Apply(bond::MapTo<MyView>(view, mappings), bonded);

    return 0;    
}
