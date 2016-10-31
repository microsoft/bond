//
// This is an example of a service and proxy that exercises futures, both Boost and STL
//

// Explicitly enable Boost futures
#define BOOST_THREAD_VERSION 4

// Include auto-generated files
#include "futures_reflection.h"
#include "futures_comm.h"

#ifdef _MSC_VER
    #pragma warning(push)
    #pragma warning(disable: 4100) // boost/thread/future.hpp(410): warning C4100: 'lk' : unreferenced formal parameter
    #pragma warning(disable: 4457) // boost/thread/future.hpp(4770): warning C4457: declaration of 'policy' hides function parameter
#include <boost/thread/future.hpp>
    #pragma warning(pop)
#else
#include <boost/thread/future.hpp>
#endif

#include <future>

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

using namespace examples::futures;

// Implement service using Boost futures
struct ServiceImpl : Service::Using<boost::promise>
{
    boost::future<bond::comm::message<Result>>
    Method(const bond::comm::payload<Param>& input) override
    {
        return boost::async([=]() -> bond::comm::message<Result>
            {
                Result result;
                result.v.emplace_back(input.value().Deserialize());
                return std::move(result);
            });
    }
};

int main()
{
    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::epoxy::EpoxyTransport transport;

    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    // Use proxy with STL futures
    Service::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    Param param;
    param.n = 123;
    param.str = "test";

    Result result = proxy.Method(param).get().value().Deserialize();
    assert(1 == result.v.size());
    assert(param == result.v.front());

    return 0;
}
