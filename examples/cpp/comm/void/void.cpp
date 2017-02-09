
//
// This is an example of a service, that exports void method with no arguments.
//

// Include auto-generated files.
#include "void_reflection.h"
#include "void_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>

using namespace examples::Void;

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
    bond::comm::SocketAddress loopback("127.0.0.1", EXAMPLE_PORT_1);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    // Use proxy with STL futures
    Service::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    bond::comm::message<void> response = proxy.Method().get();
    
    // Not an error
    assert(!response.is_error());

    return 0;
}


