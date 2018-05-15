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

#include <boost/mpl/list.hpp>

using grpc::Channel;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace examples::grpc_dll;

struct TestServiceImpl : TestService<uint32_t>::Service
{
    void TestMethod(bond::ext::gRPC::unary_call<
                        bond::bonded<MyStruct>,
                        Item<uint32_t>> call) override
    {
        MyStruct request = call.request().Deserialize();

        Item<uint32_t> response;
        response = request.items[0];

        call.Finish(response);
    }
};

struct print_metadata {
    template<typename T> void operator()(const T&) {
        std::cout << T::metadata.name << std::endl;
    }
};

int main()
{
    { // Exercise Core facilities

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
        bond::Metadata myMetadata = MyStruct::Schema::GetMetadata();

        std::cout << myMetadata.name << std::endl;

        bond::RuntimeSchema schema = bond::GetRuntimeSchema<MyStruct>();

        std::cout << schema.GetSchema().structs[schema.GetSchema().root.struct_def].fields[0].metadata.name << std::endl;

        print_metadata()(TestService<uint32_t>::Schema());

        boost::mpl::for_each<TestService<uint32_t>::Schema::methods>(print_metadata());
    }

    { // Exercise gRPC facilities

        const std::string server_address("127.0.0.1:50051");

        // Create and start a service instance
        TestServiceImpl service;
        auto server = bond::ext::gRPC::server_builder{}
            .AddListeningPort(server_address, grpc::InsecureServerCredentials())
            .RegisterService(&service)
            .BuildAndStart();

        // Create a proxy
        TestService<uint32_t>::Client proxy(
            grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
            std::make_shared<bond::ext::gRPC::io_manager>());
    }

    return 0;
}
