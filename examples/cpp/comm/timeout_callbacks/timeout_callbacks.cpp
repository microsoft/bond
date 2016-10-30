//
// This is an example of possible timeout handling.
//

// Include auto-generated files.
#include "timeout_callbacks_reflection.h"
#include "timeout_callbacks_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>
#include <bond/comm/timeout.h>

// Include helpers
#include <future>

using namespace examples::timeout_callbacks;

// Implement service
class ServiceImpl : public Service
{
    bond::comm::thread_pool threads;

    void Method(const bond::comm::payload<void>&,
                const std::function<void(const bond::comm::message<void>&)>& callback) override
    {
        threads.schedule([callback](){
            // Wait couple of seconds, to let client trigger timeout.
            std::this_thread::sleep_for(std::chrono::milliseconds(5000));
            callback(bond::comm::message<void>());
        });
    }
};


int BOND_CALL main()
{
    bond::comm::thread_pool threads;
    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    Service::Proxy proxy(transport.Connect(loopback));

    std::promise<bond::comm::message<void>> promise;
    std::future<bond::comm::message<void>> result = promise.get_future();

    uint32_t timeoutMS = 1000;
    proxy.Method(bond::comm::schedule_timeout<void>(
        threads.get_io_service(),
        [&promise](const bond::comm::message<void>& response) {
            promise.set_value(response);
        },
        timeoutMS));

    bond::comm::message<void> response = result.get();

    // Expect an error
    assert(response.is_error());
    assert(bond::comm::ErrorCode::TIMEOUT_ERROR == response.err().error_code());

    return 0;
}
