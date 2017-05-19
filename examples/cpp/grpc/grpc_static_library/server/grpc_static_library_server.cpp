// The program uses a generated Bond service PingPong; however the project
// doesn't contain any .bond file. Instead it includes
// grpc_static_library_apply.h and grpc_static_library_grpc.h and links to
// grpc_static_library.lib which contain definition of PingPong and
// pre-built Bond de/serialization code for it. This approach is useful when
// multiple projects use the same Bond types; it allows compiling Bond code
// once and distributing as a binary .lib file. The static library needs to
// be rebuilt only when .bond file (i.e. data schema) changes. Note that the
// static library and the programs that consume it have to use the same
// version of Bond.
//
// This program tests the use of the server-side service implementation.

// Must include the _apply.h file to use the pre-compiled routines;
// otherwise they'll be expanded again in this compilation unit.
#include <grpc_static_library_apply.h>
#include <grpc_static_library_reflection.h>
#include <grpc_static_library_grpc.h>

#include <bond/core/bond.h>
#include <bond/core/reflection.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/protocol/compact_binary.h>
#include <bond/stream/output_buffer.h>

#include <memory>

using grpc::Status;
using grpc::StatusCode;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace examples::grpc_static_library;

class PingPongServiceImpl final : public PingPong::Service
{
    void Ping(
        bond::ext::gRPC::unary_call<
        bond::bonded<::examples::grpc_static_library::PingRequest>,
        bond::bonded<::examples::grpc_static_library::PingResponse>> call) override
    {
        PingRequest request = call.request().Deserialize();

        PingResponse response;
        response.Payload = request.Payload;

        call.Finish(bond::bonded<PingResponse> { response }, Status::OK);
     }
};

int main()
{
    { // Serialize and deserialize
        PingRequest obj;

        obj.Payload = "ping0";

        bond::OutputBuffer buffer;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
        bond::Serialize(obj, writer);

        bond::blob data = buffer.GetBuffer();

        PingRequest obj2;

        bond::CompactBinaryReader<bond::InputBuffer> reader(data);
        bond::Deserialize(reader, obj2);
    }

    { // Access metadata
        bond::Metadata myMetadata = PingRequest::Schema::GetMetadata();
    }

    { // Create and start a service
        PingPongServiceImpl service;

        bond::ext::thread_pool threadPool;
        bond::ext::gRPC::server_builder builder(&threadPool);
        const std::string server_address("127.0.0.1:50051");
        builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
        builder.RegisterService(&service);
        std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());
    }

    return 0;
}
