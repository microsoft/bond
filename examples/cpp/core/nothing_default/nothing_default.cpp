#include "nothing_default_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::nothing_default;

template <typename T>
bond::blob Marshal(const T& obj)
{
    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
    bond::Marshal(obj, writer);
    return buffer.GetBuffer();
}

template <typename T>
void Unmarshal(const bond::InputBuffer& buffer, T& obj)
{
    bond::Unmarshal(buffer, obj);
}


int main()
{
    Struct_v1 v1;

    // Struct_v1 has a required field foo which by default is set to 'nothing'.
    // If we try to serialize object v1 w/o initializing the field to some value
    // Bond will throw an exception.

    try
    {
        printf("Serializing v1... ");
        Marshal(v1);
    }
    catch(const bond::CoreException& e)
    {
        printf("%s\n", e.what());
    }

    // Initialize field by assigning a value to it...
    v1.foo = 10;

    // ... or for complex fields using set_value() method
    v1.baz.set_value();
    v1.baz.value().push_back("test1");
    v1.baz.value().push_back("test2");

    // We can also set a field to 'nothing' using set_nothing() method.
    // Optional fields that are set to 'nothing' are omitted when object is
    // serialized.
    v1.baz.set_nothing();

    bond::InputBuffer buffer = Marshal(v1);

    // Deserialize the payload into object of type Struct_v2
    Struct_v2 v2;
    Unmarshal(buffer, v2);

    // Struct_v2 has an optional field bar, which didn't exist in Struct_v1.
    // It is initialized to 'nothing' by default. By checking if the field is
    // 'nothing' after de-serialization we can detect if it was present in the
    // payload or not.

    if (v2.baz.is_nothing())
    {
        printf("Field 'baz' was not present in the payload\n");
    }

    // Accessing field that is set to 'nothing' throws and exception
    try
    {
        double d;

        printf("Using field bar... ");
        d = v2.bar.value();
        fprintf(stderr, "Accessing field bar should have thrown, but we got %g instead", d);
        exit(1); // should not have gotten here
    }
    catch(const bond::CoreException& e)
    {
        printf("%s\n", e.what());
    }

    return 0;
}
