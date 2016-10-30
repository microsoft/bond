
//
// This is an example of an explicit event processing of epoxy transport in user thread.
//

// Include auto-generated files.
#include "epoxy_io_service_reflection.h"
#include "epoxy_io_service_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>

using namespace examples::epoxy_io_service;

// Implement service 
class ServiceImpl : public Service
{
    void Method(const bond::comm::payload<void>&,
                const std::function<void(const bond::comm::message<void>&)>& callback) override
    {
        // Respond right away, with void message
        callback(bond::comm::message<void>());
    }
};


int BOND_CALL main()
{
    auto thread_service = boost::make_shared<bond::comm::ThreadService>();

    bond::comm::SocketAddress loopback("127.0.0.1", 25188);

    // Construct epoxy transport instance against explicit io_service
    bond::comm::epoxy::EpoxyTransport transport(thread_service);

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    // Use proxy with STL futures
    Service::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    std::future<bond::comm::message<void>> result = proxy.Method();

    // Run io_service in this thread until result is ready
    while (std::future_status::ready != result.wait_for(std::chrono::milliseconds(1)))
    {
        // Poll and process ready events
        thread_service->io_service()->poll();
    }

    bond::comm::message<void> response = result.get();
    
    // Not an error
    assert(!response.is_error());

    return 0;
}


