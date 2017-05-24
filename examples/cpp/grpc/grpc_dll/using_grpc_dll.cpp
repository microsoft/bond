// The program uses a generated Bond service TestService; however the
// project doesn't contain any .bond file. Instead it includes headers from
// and links to grpc_dll_example.dll which contain definition of TestService
// and pre-built Bond de/serialization code for it. This approach is useful
// when multiple projects use the same Bond types; it allows compiling Bond
// code once and distributing as a binary .lib file. The static library
// needs to be rebuilt only when .bond file (i.e. data schema) changes. Note
// that the DLL and the programs that consume it have to use the same
// version of Bond.
//
// This program tests the use of the gRPC client and service code.

// Must include the _apply.h file to use the pre-compiled routines;
// otherwise they'll be expanded again in this compilation unit.
#include <grpc_dll_apply.h>

// Reflection header needed only for explicit metadata access -- don't
// include by default as it will increase your build times
#include <grpc_dll_reflection.h>

#include <grpc_dll_grpc.h>

#include <bond/core/bond.h>
#include <bond/core/reflection.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/protocol/compact_binary.h>
#include <bond/stream/output_buffer.h>

#include <iostream>
#include <memory>

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;
using grpc::StatusCode;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace examples::grpc_dll;

struct TestServiceImpl : TestService::Service
{
    void TestMethod(bond::ext::gRPC::unary_call<
                        bond::bonded<MyStruct>,
                        bond::bonded<Item>> call) override
    {
        MyStruct request = call.request().Deserialize();

        Item response;
        response = request.items[0];

        call.Finish(bond::bonded<Item> { response }, Status::OK);
    }
};

int main()
{
    { // Exercise Core facilities

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
    }

    { // Exercise gRPC facilities

        const std::string server_address("127.0.0.1:50051");

        auto ioManager = std::make_shared<bond::ext::gRPC::io_manager>();
        bond::ext::thread_pool threadPool;

        // Create and start a service instance
        TestServiceImpl service;
        bond::ext::gRPC::server_builder builder(&threadPool);
        builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
        builder.RegisterService(&service);
        std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

        // Create a proxy
        TestService::Client proxy(
            grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
            ioManager,
            &threadPool);
    }

    return 0;
}
