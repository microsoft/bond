//
// This is an example of pingpong service
//

// Include auto-generated files
#include "pingpong_reflection.h"
#include "pingpong_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>

using namespace Bond::Examples::PingPong;

// Implement service
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
    bond::comm::SocketAddress loopback("127.0.0.1", TEST_PORT_1);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<PingPongImpl>());

    PingPong::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    PingRequest request;
    request.Payload = "ping0";

    PingResponse response = proxy.Ping(std::move(request)).get().value().Deserialize();

    assert(response.Payload == request.Payload);

    return 0;
}
