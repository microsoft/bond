
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <unit_test_comm_reflection.h>
#include <unit_test_comm_comm.h>

#include "transport_list.h"
#include <bond/comm/layers.h>
#include <bond/comm/timeout.h>
#include <bond/comm/thread_pool.h>

#include <boost/mpl/for_each.hpp>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"
#include "bonded_cast.h"
#include "test_utils_comm.h"
#include <boost/chrono.hpp>
#include <string.h>
#include <atomic>

using namespace unittest::comm;

template <typename T>
class JumboTransportTests
{
    typedef T Transport;

    class First_ServiceImpl
        : public First
    {
        //
        // implement First_Service
        //
        void Method1(const bond::comm::payload<Params>& input,
                     const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Params p;
            input.value().Deserialize(p);

            switch (p.op)
            {
                case FetchCallback:
                    {
                        //
                        // Return r.z = 1 immediately.
                        //
                        Result r;
                        r.v.resize(p.x, p.y);
                        return callback(boost::cref(r));
                    }

                case PostCallback:
                    {
                        //
                        // Return r.z = 1 immediately.
                        //
                        Result r;
                        r.z = static_cast<uint32_t>(p.v.size());
                        for (bond::blob b : p.b)
                        {
                            r.z += b.size();
                        }
                        return callback(boost::cref(r));
                    }

                default:
                {
                    UT_AssertIsTrue(false);
                }
            }
        }

    public:

        First_ServiceImpl()
        {}
    };


    static
    void Server1Service1Receive1k()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Size0")
        {
            Params params;
            Result result;

            params.op = FetchCallback;
            params.x = 1024;
            params.y = 123456;


            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(static_cast<uint32_t>(result.v.size()) == params.x);
        }
    }


    static
    void Server1Service1Receive10k()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Receive10k")
        {
            Params params;
            Result result;

            params.op = FetchCallback;
            params.x = 10 * 1024;
            params.y = 123456;


            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(static_cast<uint32_t>(result.v.size()) == params.x);
        }
    }

    static
    void Server1Service1Receive1M()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        ONCE("Server1Service1Receive1M")
        {
            Params params;
            Result result;

            params.op = FetchCallback;
            params.x = 1024 * 1024;
            params.y = 123456;


            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(static_cast<uint32_t>(result.v.size()) == params.x);
        }
    }


    static
    void Server1Service1Receive10M()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        ONCE("Server1Service1Receive10M")
        {
            Params params;
            Result result;

            params.op = FetchCallback;
            params.x = 10*1024*1024;
            params.y = 123456;


            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(static_cast<uint32_t>(result.v.size()) == params.x);
        }
    }

    static
    void Server1Service1Send1k()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send1k")
        {
            Params params;
            Result result;

            params.op = PostCallback;
            params.v.resize(1024, 123456);

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == static_cast<uint32_t>(params.v.size()));
        }
    }

    static
    void Server1Service1Send10k()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send10k")
        {
            Params params;
            Result result;

            params.op = PostCallback;
            params.v.resize(10*1024, 123456);

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == static_cast<uint32_t>(params.v.size()));
        }
    }

    static
    void Server1Service1Send10000k()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        ONCE("Server1Service1Send10000k")
        {
            Params params;
            Result result;

            params.op = PostCallback;
            params.v.resize(10*1024*1024, 123456);

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == static_cast<uint32_t>(params.v.size()));
        }
    }


    static
    void Server1Service1Send100B()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect one pair of First::Proxy and Second::Proxy to 9000 and
        // another pair to 9001 via independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send100B")
        {
            Params params;
            Result result;

            params.op = PostCallback;
            params.b.resize(100, bond::blob(boost::make_shared<char[]>(1024), 1024));

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == static_cast<uint32_t>(params.b.size()) * 1024);
        }
    }

public:

    static
    void Initialize()
    {
        std::string name = typeid(Transport).name();

        // Shorten the name.
        boost::algorithm::replace_all(name, "struct ", "");
        boost::algorithm::replace_all(name, "class ", "");
        boost::algorithm::replace_all(name, "bond::transport::", "");
        boost::algorithm::replace_all(name, "bond::", "");

        UnitTestSuite suite(name.c_str());

        suite.AddTestCase(Server1Service1Receive1k, "Server1Service1Receive1k");

        suite.AddTestCase(Server1Service1Receive10k, "Server1Service1Receive10k");

        suite.AddTestCase(Server1Service1Receive1M, "Server1Service1Receive1M");

        suite.AddTestCase(Server1Service1Receive10M, "Server1Service1Receive10M");

        suite.AddTestCase(Server1Service1Send1k, "Server1Service1Send1k");

        suite.AddTestCase(Server1Service1Send10k, "Server1Service1Send10k");

        suite.AddTestCase(Server1Service1Send10000k, "Server1Service1Send10000k");

        suite.AddTestCase(Server1Service1Send100B, "Server1Service1Send100B");
    }
};


void init_unit_test_comm_jumbo()
{
    InitializeTests<JumboTransportTests, bond::comm::FastWireProtocol>();
}
