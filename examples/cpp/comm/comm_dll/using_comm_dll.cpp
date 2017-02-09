// The program uses a generated Bond service TestService; however the project
// doesn't contain any .bond file. Instead it includes headers from
// and links to comm_dll_example.dll which contain definition of TestService and
// pre-built Bond de/serialization code for it. This approach is useful when
// multiple projects use the same Bond types; it allows compiling Bond code
// once and distributing as a binary .lib file. The static library needs to be
// rebuilt only when .bond file (i.e. data schema) changes.  Note that the
// DLL and the programs that consume it have to use the same version
// of Bond.
// This program tests the use of the server-side service implementation.
#include <comm_dll_apply.h>

// Reflection header needed only for explicit metadata access -- don't
// include by default as it will increase your build times
#include <comm_dll_reflection.h>

#include <comm_dll_comm.h>

// We still need to include Bond headers, however only small inline functions
// will be compiled as part of this file and expensive to build de/serialization
// code will be linked in from static_library.lib.
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

#include <bond/comm/transport/epoxy.h>

#include <iostream>

using namespace examples::comm_dll;

struct TestServiceImpl : TestService
{
    void TestMethod(const bond::comm::payload<MyStruct>& request,
                    const std::function<void(const bond::comm::message<Item>&)>& callback) override
    {
        Item response;
        response = request.value().Deserialize().items[0];
        callback(std::move(response));
    }
};

int main()
{
    // Exercise Core facilities

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

    // Access metadata
    bond::Metadata myMetadata = MyStruct::Schema::GetMetadata();

    std::cout << myMetadata.name << std::endl;

    bond::RuntimeSchema schema = bond::GetRuntimeSchema<MyStruct>();

    std::cout << schema.GetSchema().structs[schema.GetSchema().root.struct_def].fields[0].metadata.name << std::endl;

    // Exersize Comm facilities

    bond::comm::SocketAddress loopback("127.0.0.1", EXAMPLE_PORT_1);
    bond::comm::epoxy::EpoxyTransport transport;

    // Create a service instance
    auto serverinstance = boost::make_shared<TestServiceImpl>();
    auto server = transport.Bind(loopback, serverinstance);

    // Create a proxy instance
    TestService::Proxy proxy(transport.Connect(loopback));

    return 0;
}
