#include "import_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::imports;

int main()
{
    // Import is used to reference types defined in another .bond file. 
    // Usually imported .bond files are compiled separately in another project,
    // which then provides a static .lib and header files generated from the
    // .bond files using Bond CodeGen.
    // In this example we import bond::TypeDef struct and bond::BondDataType enum
    // which are defined in bond.bond shipping with Bond. Our example links to 
    // bond.lib. The directory containing header files generated from bond.bond
    // is by default in include path for Bond projects. 
    Struct obj, obj2;

    obj.type.id = bond::BT_STRING;

    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    // Serialize the object
    bond::Serialize(obj, writer);

    bond::blob buffer = output.GetBuffer();
    bond::CompactBinaryReader<bond::InputBuffer> reader(buffer);
    
    // De-serialize the object
    bond::Deserialize(reader, obj2);
    
    return 0;    
}
