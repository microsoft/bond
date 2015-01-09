#include "generics_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::generics;


int main()
{
    Struct<std::string, double, Base<std::wstring> > obj, obj2;

    obj.n.x = L"test";
    obj.x = 0.11;  
    obj.y = "test";  
    obj.items.push_back(3.14);
    obj.items.push_back(0);

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
