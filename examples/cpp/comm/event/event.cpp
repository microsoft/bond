
//
// This is an example of a service, that waits for events.
//

// Include auto-generated files.
#include "event_reflection.h"
#include "event_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>

using namespace examples::event;

// Implement service 
class ServiceImpl : public Service
{
    void Poke(const bond::comm::payload<Signal>& input) override
    {
        // Store incoming signal
        m_promise.set_value(input.value().Deserialize());
    }

    std::promise<Signal> m_promise;
public:

    ServiceImpl(std::promise<Signal>&& promise)
        : m_promise(std::move(promise))
    {}
};

int BOND_CALL main()
{
    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::epoxy::EpoxyTransport transport;

    // Setup event confirmation
    std::promise<Signal> promise;
    std::future<Signal> future = promise.get_future();

    // Instantiate service to be published
    ServiceImpl service(std::move(promise));

    auto server = transport.Bind(loopback, boost::ref(service));

    // Signal to send
    Signal signal;
    signal.x = 42;

    Service::Proxy proxy(transport.Connect(loopback));
    proxy.Poke(signal);

    // Not an error, expected signal received by server.
    assert(42 == future.get().x);

    return 0;
}


