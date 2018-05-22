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
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/protocol/compact_binary.h>
#include <bond/stream/output_buffer.h>

#include <memory>

using namespace examples::grpc_static_library;

class PingPongServiceImpl final : public PingPong::Service
{
public:
    using PingPong::Service::Service;

private:
    void Ping(bond::ext::grpc::unary_call<PingRequest, PingResponse> call) override
    {
        PingRequest request = call.request().Deserialize();

        PingResponse response;
        response.Payload = request.Payload;

        call.Finish(response);
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
        bond::ext::grpc::thread_pool threadPool;
        std::unique_ptr<PingPongServiceImpl> service{ new PingPongServiceImpl{ threadPool } };

        const std::string server_address("127.0.0.1:50051");

        ::grpc::ServerBuilder builder;
        builder.AddListeningPort(server_address, ::grpc::InsecureServerCredentials());

        auto server = bond::ext::grpc::server::Start(builder, std::move(service));
    }

    return 0;
}
