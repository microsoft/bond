#include <bond/core/tuple.h>

int main()
{
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    std::string name = "James";
    std::list<double> numbers = {3.14};
    
    // Pack is a variadic function and can accept any number of arguments of
    // any types supported by Bond, including containers and user defined Bond
    // structs.
    Pack(writer, name, numbers, 10);

    bond::blob payload = output.GetBuffer();

    // Unpack all values
    {
        bond::CompactBinaryReader<bond::InputBuffer> reader(payload);

        int n = 0;
        std::list<double> numbers1;
        std::string name1;

        // Unpack deserializes payload into variables passed by reference.
        Unpack(reader, name1, numbers1, n);

        assert(n == 10);
        assert(name == name1);
        assert(numbers == numbers1);
    }

    // Partial unpack (aka pack view)
    {
        bond::CompactBinaryReader<bond::InputBuffer> reader(payload);

        std::list<double> numbers1;

        // Since we haven't explicitly defined a schema, values are identified
        // implicitly by their position in the Pack/Unpack call, similarly to
        // items of a tuple. Items at the end are automatically ignored if no
        // variable is provided during unpacking. Items in the middle can be
        // ignored explicitly by passing std::ignore to Unpack.
        Unpack(reader, std::ignore, numbers1);

        assert(numbers == numbers1);
    }

    return 0;
}

