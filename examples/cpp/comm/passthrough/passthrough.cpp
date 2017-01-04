//
// This is an example of two service chain: middle service passes all RPC calls unmodified to next service.
//

// Include auto-generated files
#include "passthrough_reflection.h"
#include "passthrough_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>
#include <boost/core/ignore_unused.hpp>

using namespace examples::passthrough;

// Implement middle tier service
class IntermediateImpl : public Intermediate
{
    void Pass(const bond::comm::payload<Param>& input,
              const std::function<void(const bond::comm::message<Result>&)>& callback) override
    {
        // Pass incoming request and callback downstream
        m_worker.Process(input, callback);
    }

    Service::Proxy m_worker;

public:

    IntermediateImpl(const Service::Proxy& worker)
        : m_worker(worker)
    {}
};


// Implement worker
class ServiceImpl : public Service
{
    void Process(const bond::comm::payload<Param>& input,
                 const std::function<void(const bond::comm::message<Result>&)>& callback) override
    {
        Param param = input.value().Deserialize();

        // Process request, dispatch response

        Result result;
        result.x = param.x;
        callback(std::move(result));
    }
};

int BOND_CALL main()
{
    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::SocketAddress intermediate("127.0.0.1", 25189);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());
    Service::Proxy worker(transport.Connect(loopback));

    auto server2 = transport.Bind(intermediate, boost::make_shared<IntermediateImpl>(worker));

    // Connect to intermediate service
    Intermediate::Proxy::Using<std::promise> proxy(transport.Connect(intermediate));

    Param param;
    param.x = 42;

    Result result = proxy.Pass(param).get().value().Deserialize();

    boost::ignore_unused(result);
    assert(42 == result.x);

    return 0;
}
