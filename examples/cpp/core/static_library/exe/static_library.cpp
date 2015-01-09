// The program uses a generate Bond struct MyStruct, however the project
// doesn't contain any .bond file. Instead it includes static_library_apply.h
// and links to static_library.lib which contain definition of MyStruct and
// pre-built Bond de/serialization code for it. This approach is useful when
// multiple projects use the same Bond types; it allows to compile Bond code
// once and distribute as a binary .lib file. The static library needs to be
// rebuilt only when .bond file (i.e. data schema) changes.  Note that the
// static library and the programs that consume it have to use the same version
// of Bond. 
#include <static_library_apply.h>

// We still need to include Bond headers, however only small inline functions
// will be compiled as part of this file and expensive to build de/serialization
// code will be linked in from static_library.lib.
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::static_library;


int main()
{
    MyStruct obj;

    // Initialize
    obj.items.resize(1);
    obj.items[0].numbers.push_back(13);

    Item item;

    item.numbers.push_back(11);
    obj.item = bond::bonded<Item>(item);

    // Serialize
    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
    bond::Serialize(obj, writer);
        
    bond::blob data = buffer.GetBuffer();

    MyStruct obj2;

    // Deserialize
    bond::CompactBinaryReader<bond::InputBuffer> reader(data);
    bond::Deserialize(reader, obj2);

    Item item2;
    
    obj2.item.Deserialize(item2);
    
    return 0;    
}
