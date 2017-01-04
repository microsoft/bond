//
// This is an example of responding with an error.
//

// Include auto-generated files
#include "error_reflection.h"
#include "error_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>

using namespace examples::error;

// Define custom error
const int32_t MY_ERROR_CODE = 42;
const std::string MY_ERROR_MESSAGE = "My custom error message";

// Implement service
class ServiceImpl : public Service
{
    void Method(const bond::comm::payload<Param>&,
                const std::function<void (const bond::comm::message<Result>&)>& callback) override
    {
        //
        // Respond with prefered error
        //
        callback(bond::comm::error(MY_ERROR_CODE, MY_ERROR_MESSAGE));
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

    // Expect an error
    assert(response.is_error());

    // Expect custom error.
    assert(MY_ERROR_CODE == response.err().error_code());
    assert(MY_ERROR_MESSAGE == response.err().message());

    try
    {
        // Try to access payload.
        response.value().Deserialize();

        // Code execution doesn't reach this assert
        assert(false);
    }
    catch (const bond::comm::CommException& ex)
    {
        boost::ignore_unused_variable_warning(ex);
        // Expect InvalidInvocation when trying to access error payload.
        assert(bond::comm::ErrorCode::INVALID_INVOCATION == ex.error_code);
    }

    return 0;
}
