#include "access_control_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::access_control;


template <typename T>
bond::blob Serialize(const T& obj)
{
    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    bond::Serialize(obj, writer);

    return buffer.GetBuffer();
}


void MakeInput(bond::blob& data, bond::blob& schema)
{
    Company obj;

    obj.ticker = "MSFT";
    obj.city = "Redmond";

    obj.ceo.name = "John Doe";
    obj.ceo.address = "Main Campus";

    obj.employees.resize(2);

    obj.employees[0].name = "Bob";
    obj.employees[0].address = "Main Campus";

    obj.employees[1].name = "Alice";
    obj.employees[1].address = "City Center";

    data = Serialize(obj);
    schema = Serialize(bond::GetRuntimeSchema<Company>());
}


void ConsumeOutput(const bond::blob& buffer)
{
    Company obj;

    bond::CompactBinaryReader<bond::InputBuffer> reader(buffer);
    bond::Deserialize(reader, obj);
}


// The transform filters fields from the input data using attributes and
// specified ACL. Note that to keep the example simple, the transform passes
// through all unknown fields (i.e. fields not present in the provided runtime
// schema).  Fields that don't pass ACL check are omitted from the output. This
// works fine for optional fields. For required fields the transform could
// output default value, which is available in field meta data.
template <typename Writer>
class AccessControlFilter
    : public bond::Serializer<Writer>
{
public:
    AccessControlFilter(Writer& output, const char* ACL, bool base = false)
        : bond::Serializer<Writer>(output, base),
          _output(output),
          _ACL(ACL)
    {}

    // Base struct
    template <typename T>
    bool Base(const T& value) const
    {
        // Apply the filter to the fields of the base
        Apply(AccessControlFilter(_output, _ACL, true), value);
        return false;
    }

    // Field
    template <typename T>
    bool Field(uint16_t id, const bond::Metadata& metadata, const T& value) const
    {
        if (CheckACL(metadata))
        {
            _output.WriteFieldBegin(GetTypeId(value), id, metadata);
            Write(value);
            _output.WriteFieldEnd();
        }

        return false;
    }

    // Elements of a container
    template <typename T>
    void Container(const T& element, uint32_t size) const
    {
        _output.WriteContainerBegin(size, GetTypeId(element));

        while (size--)
            Write(element);

        _output.WriteContainerEnd();
    }

    template <typename Key, typename T>
    void Container(const Key& key, const T& value, uint32_t size) const
    {
        _output.WriteContainerBegin(size, std::make_pair(GetTypeId(key), GetTypeId(value)));

        while (size--)
        {
            Write(key);
            Write(value);
        }

        _output.WriteContainerEnd();
    }

private:
    bool CheckACL(const bond::Metadata& metadata) const
    {
        std::map<std::string, std::string>::const_iterator it;

        // Check if the field has ACL attribute. If it doesn't we will default
        // to "not-accessible".
        it = metadata.attributes.find("ACL");

        // Allow access if the ACL attribute matches or specifies "all" access
        return it != metadata.attributes.end()
            && (it->second == _ACL || it->second == "all");
    }


    template <typename Reader, typename T>
    typename boost::enable_if<bond::is_basic_type<T> >::type
    Write(const bond::value<T, Reader>& value) const
    {
        T data;

        value.Deserialize(data);
        _output.Write(data);
    }


    // Apply filter recursively to nested fields and containers
    template <typename Reader, typename T>
    typename boost::disable_if<bond::is_basic_type<T> >::type
    Write(const bond::value<T, Reader>& value) const
    {
        Apply(AccessControlFilter(_output, _ACL), value);
    }


    template <typename Reader, typename T>
    void Write(const bond::bonded<T, Reader>& value) const
    {
        Apply(AccessControlFilter(_output, _ACL), value);
    }


    Writer&     _output;
    const char* _ACL;
};


// FilterByACL
template <typename Writer>
AccessControlFilter<Writer> FilterByACL(Writer& output, const char* ACL)
{
    return AccessControlFilter<Writer>(output, ACL);
}


int main()
{
    // In order to make the example self contained, we generate blobs with
    // serialized data and runtime schema. Normally these would come from
    // outside, e.g. from a file or over network.
    // Note that the rest of the code doesn't assume compile-time knowledge of
    // the type of the object, the code only needs runtime schema, which can be
    // provided at runtime.
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

    // bond::bonded<void> binds the protocol reader with the schema of the object
    // contained within the serialized data.
    bond::bonded<void> input(reader, schema);

    // We apply a transform to the data to filter fields based on ACL attributes.
    // The result is blob of serialized data which only includes fields to which
    // we have access.

    const char* ACLs[] = {"identity", "location", "nothing"};

    for (size_t i = 0; i < sizeof(ACLs)/sizeof(*ACLs); ++i)
    {
        bond::OutputBuffer                              output;
        bond::CompactBinaryWriter<bond::OutputBuffer>   writer(output);

        // Filter fields from input by ACL, and write results to output
        bond::Apply(FilterByACL(writer, ACLs[i]), input);

        ConsumeOutput(output.GetBuffer());
    }

    return 0;
}
