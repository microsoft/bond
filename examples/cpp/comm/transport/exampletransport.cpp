//
// This is an example of a custom transport implementation
//

// Explicitly enable Boost futures
#define BOOST_THREAD_VERSION 4

// Include auto-generated files.
#include "exampletransport_reflection.h"
#include "exampletransport_comm.h"

#ifdef _MSC_VER
    #pragma warning(push)
    #pragma warning(disable: 4100) // boost/thread/future.hpp(410): warning C4100: 'lk' : unreferenced formal parameter
#include <boost/thread/future.hpp>
    #pragma warning(pop)
#else
#include <boost/thread/future.hpp>
#endif


// Include preferred transport: ExampleTransport in this case
#include "exampletransport.h"

using namespace examples::transport;

// Implement service using boost futures
struct ServiceImpl : Service
{
    void Method(const bond::comm::payload<Param>& input,
                const std::function<void(const bond::comm::message<Result>&)>& callback) override
    {
        Result result;
        result.v.emplace_back(input.value().Deserialize());

        callback(std::move(result));
    }
};

int main()
{
    bond::comm::SocketAddress loopback("127.0.0.1", EXAMPLE_PORT_1);
    examples::ExampleTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());
    
    // Use proxy with std futures
    Service::Proxy::Using<boost::promise> proxy(transport.Connect(loopback));

    Param param;
    param.n = 13;
    param.str = "test";
    
    auto future = proxy.Method(param);

    // pump messages
    //
    while (transport.PumpEvent())
    {}

    Result result = future.get().value().Deserialize();
    assert(!result.v.empty());
    assert(result.v.front() == param);

    return 0;    
}
