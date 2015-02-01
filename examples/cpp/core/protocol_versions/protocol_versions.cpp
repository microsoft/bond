#include <bond/core/bond_version.h>

namespace bond
{
    // Normally protocols default to version 1 for backward compatibility. 
    // By specializing default_version meta-function we can change default
    // version. In this program Simple Protocol will default to version 2.
    template <typename Buffer> struct
    default_version<SimpleBinaryReader<Buffer> >
    {
        static const uint16_t value = v2;
    };
}

#include "protocol_versions_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::protocol_versions;

int main()
{
    Struct obj;

    obj.n = 0x1000;
    obj.str = "test";
    obj.items.push_back(3.14);
    obj.items.push_back(0);

    {
        // If we don't pass explicit protocol version to ctor, then implicit 
        // default version is used. 
        bond::OutputBuffer output;
        bond::SimpleBinaryWriter<bond::OutputBuffer> writer(output);
        bond::Serialize(obj, writer);

        Struct obj2;
        bond::InputBuffer input(output.GetBuffer());
        bond::SimpleBinaryReader<bond::InputBuffer> reader(input);
        bond::Deserialize(reader, obj2);
    }

    {
        // We can overide the default by passing version to reader/writer ctor.
        bond::OutputBuffer output;
        bond::SimpleBinaryWriter<bond::OutputBuffer> writer(output, bond::v1);
        bond::Serialize(obj, writer);

        Struct obj2;
        bond::InputBuffer input(output.GetBuffer());
        bond::SimpleBinaryReader<bond::InputBuffer> reader(input, bond::v1);
        bond::Deserialize(reader, obj2);
    }
    
    return 0;    
}
