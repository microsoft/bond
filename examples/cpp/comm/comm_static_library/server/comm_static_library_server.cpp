// The program uses a generated Bond service PingPong; however the project
// doesn't contain any .bond file. Instead it includes
// comm_static_library_apply.h and comm_static_library_comm.h
// and links to comm_static_library.lib which contain definition of PingPong and
// pre-built Bond de/serialization code for it. This approach is useful when
// multiple projects use the same Bond types; it allows compiling Bond code
// once and distributing as a binary .lib file. The static library needs to be
// rebuilt only when .bond file (i.e. data schema) changes.  Note that the
// static library and the programs that consume it have to use the same version
// of Bond.
// This program tests the use of the server-side service implementation.
#include <comm_static_library_apply.h>
#include <comm_static_library_reflection.h>
#include <comm_static_library_comm.h>

// We still need to include Bond headers, however only small inline functions
// will be compiled as part of this file and expensive to build de/serialization
// code will be linked in from static_library.lib.
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/comm/transport/epoxy.h>

#include <future>

using namespace examples::comm_static_library;

struct PingPongImpl : PingPong
{
    void Ping(const bond::comm::payload<PingRequest>& request,
              const std::function<void(const bond::comm::message<PingResponse>&)>& callback) override
    {
        PingResponse response;
        response.Payload = request.value().Deserialize().Payload;
        callback(std::move(response));
    }
};

int main()
{
    PingRequest obj;

    obj.Payload = "ping0";

    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
    bond::Serialize(obj, writer);

    bond::blob data = buffer.GetBuffer();

    PingRequest obj2;

    // Deserialize
    bond::CompactBinaryReader<bond::InputBuffer> reader(data);
    bond::Deserialize(reader, obj2);

    // Access metadata
    bond::Metadata myMetadata = PingRequest::Schema::GetMetadata();

    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<PingPongImpl>());

    return 0;
}
