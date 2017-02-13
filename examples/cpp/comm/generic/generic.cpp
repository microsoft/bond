//
// This is an example of generic service
//

// Include auto-generated files
#include "generic_reflection.h"
#include "generic_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>
#include <boost/core/ignore_unused.hpp>

using namespace examples::generic;

// Implement service
struct ServiceImpl : Service<Result, Param>
{
    void Method(const bond::comm::payload<Param>& input,
                const std::function<void(const bond::comm::message<Result>&)>& callback) override
    {
        Result result;
        result.z = input.value().Deserialize().x;
        callback(std::move(result));
    }
};

int main()
{
    bond::comm::SocketAddress loopback("127.0.0.1", TEST_PORT_1);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    Service<Result, Param>::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    Param param;
    param.x = 42;

    Result result = proxy.Method(std::move(param)).get().value().Deserialize();

    boost::ignore_unused(result);
    assert(42 == result.z);

    return 0;
}
