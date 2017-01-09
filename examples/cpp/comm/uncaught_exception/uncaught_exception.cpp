//
// This is an example of a service and proxy that exercises futures, both Boost and STL
//

// Include auto-generated files
#include "uncaught_exception_reflection.h"
#include "uncaught_exception_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>
#include <boost/core/ignore_unused.hpp>

using namespace examples::uncaught_exception;

// Implement service
class ServiceImpl : public Service
{
    void Method(const bond::comm::payload<Param>&,
                const std::function<void (const bond::comm::message<Result>&)>&) override
    {
        throw std::runtime_error("BOOM");
    }
};

int BOND_CALL main()
{
    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    // Use proxy with STL futures
    Service::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    Param param;

    bond::comm::message<Result> response = proxy.Method(param).get();

    assert(response.is_error());

    // Expect InternalServerError for uncaught exceptions.
    assert(bond::comm::ErrorCode::INTERNAL_SERVER_ERROR == response.err().error_code());

    try
    {
        response.value().Deserialize();
        // Code execution shouldn't reach this assert
        assert(false);
    }
    catch (const bond::comm::CommException& ex)
    {
        boost::ignore_unused(ex);
        // Expect InvalidInvocation when trying to access error payload.
        assert(bond::comm::ErrorCode::INVALID_INVOCATION == ex.error_code);
    }

    return 0;
}
