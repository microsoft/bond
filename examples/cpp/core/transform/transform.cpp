#include "transform_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::transform;

template <typename T>
bond::blob Serialize(const T& obj)
{
    bond::OutputBuffer buffer;
    
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    bond::Serialize(obj, writer);

    return buffer.GetBuffer();
}


// Define a transform to perform custom conversion from the old schema to the
// new schema. Our transform inherits standard processing from bond::To
// transform, and only handles the field that needs to be manually converted.
template <typename T>
class ConvertTo
    : public bond::To<T>
{
public:
    typedef bond::To<T> my_base;

    ConvertTo(T& var)
        : my_base(var),
          _var(var)
    {}


    template <typename Reader>
    bool UnknownField(uint16_t id, const bond::value<std::string, Reader>& value) const
    {
        // The old version of the schema had a string field 'name', with id 20.
        // If we encounter the field in the payload, we convert it to the new
        // fields 'first_name' and 'last_name'.
        if (id == 20)
        {
            // De-serialize the value of the field.
            value.Deserialize(_var.first_name);

            // Split the name into first and last name.
            size_t space = _var.first_name.find(' ');

            _var.last_name.assign(_var.first_name.c_str() + space + 1);
            _var.first_name.resize(space);
        }

        return false;
    }

    template <typename U>
    bool UnknownField(const U&) const
    {
        return false;
    }

    template <typename U>
    bool UnknownField(uint16_t, const U&) const
    {
        return false;
    }

private:
    T& _var;
};


int main()
{
    Struct_1_0 obj1;

    obj1.n = 0x1000;
    obj1.name = "First Last";
    obj1.items.push_back(3.14);
    obj1.items.push_back(0);

    bond::blob output;
    
    // Serialize an object of old type.
    output = Serialize(obj1);

    // Construct protocol reader and bonded<Struct_2_0> using the buffer
    // containing the serialized old object.
    bond::CompactBinaryReader<bond::InputBuffer>    reader(output);
    bond::bonded<Struct_2_0>                        bonded(reader);

    Struct_2_0 obj2;
    
    // Apply ConvertTo transform to the bonded object. 
    // The transform will translate field 'name', which was used in the old
    // schema, into fields 'first_name' and 'last_name', which replaced 'name'
    // in the new version of the schema.
    bond::Apply(ConvertTo<Struct_2_0>(obj2), bonded);
    
    return 0;    
}
