// The program uses a generated Bond struct MyStruct, however the project
// doesn't contain any .bond file. Instead it includes dll_apply.h
// and links to dll_example.dll which contain definition of MyStruct and
// pre-built Bond de/serialization code for it. This approach is useful when
// multiple projects use the same Bond types; it allows compling Bond code
// once and distributing as a binary .lib file. The DLL needs to be
// rebuilt only when .bond file (i.e. data schema) changes.  Note that the
// DLL and the programs that consume it have to use the same version
// of Bond.
#include <dll_apply.h>
#include <dll_reflection.h>

// We still need to include Bond headers, however only small inline functions
// will be compiled as part of this file and expensive to build de/serialization
// code will be linked in from static_library.lib.
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

#include <iostream>

using namespace examples::dll;


int main()
{
    MyStruct obj;

    // Initialize
    obj.items.resize(1);
    obj.items[0].numbers.push_back(13);

    Item<uint32_t> item;

    item.numbers.push_back(11);
    obj.item = bond::bonded<Item<uint32_t>>(item);

    // Serialize
    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
    bond::Serialize(obj, writer);
        
    bond::blob data = buffer.GetBuffer();

    MyStruct obj2;

    // Deserialize
    bond::CompactBinaryReader<bond::InputBuffer> reader(data);
    bond::Deserialize(reader, obj2);

    Item<uint32_t> item2;
    
    obj2.item.Deserialize(item2);

    // Access metadata
    bond::Metadata myMetadata = MyStruct::Schema::metadata;

    std::cout << myMetadata.name << std::endl;

    bond::RuntimeSchema schema = bond::GetRuntimeSchema<MyStruct>();

    std::cout << schema.GetSchema().structs[schema.GetSchema().root.struct_def].fields[0].metadata.name << std::endl;

    // Use enum
    std::cout << static_cast<int>(Color::Red) << " = " << ToString(Color::Red) << std::endl;
    
    return 0;    
}
