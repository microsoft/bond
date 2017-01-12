
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
class BasicTransportTests
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

            //
            // Return r.z = 1 immediately.
            //
            Result r;
            r.z = 1;
            return callback(boost::cref(r));
        }

    public:

        First_ServiceImpl()
        {}
    };


    class Second_ServiceImpl
        : public Second
    {
        //
        // implement Second_Service
        //
        void Method2(const bond::comm::payload<Params>& input,
                     const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Params p;
            input.value().Deserialize(p);

          
            Result r;
            r.z = 2;
            callback(boost::cref(r));
        }

    public:

        Second_ServiceImpl()
        {}
    };


    static
    void Server1Service1StartStop()
    {
        //
        // Create service.
        First_ServiceImpl service;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(service));

        UT_AssertIsNotNull(server.get());

        // TODO: check bind status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server1Service2StartStop()
    {
        //
        // Create service.
        First_ServiceImpl first;
        Second_ServiceImpl second;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        bond::comm::ServiceTable table;
        table.Using(bond::comm::FastWireProtocol())
             .Register(boost::ref(first))
             .Register(boost::ref(second));

        //
        // Start transport.
        //
        Transport transport;

        // publish both services
        bond::comm::Server server = transport.Bind(address, table);

        UT_AssertIsNotNull(server.get());

        // TODO: check bind status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server2Service1StartStop()
    {
        //
        // Create service.
        //
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server1 = transport.Bind(serverAddress1, boost::ref(firstService));
        bond::comm::Server server2 = transport.Bind(serverAddress2, boost::ref(firstService));

        UT_AssertIsNotNull(server1.get());
        UT_AssertIsNotNull(server2.get());

        // TODO: check bind status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server1Service1Connect0()
    {
        //
        // Start transport.
        //
        Transport transport;

        bond::comm::SocketAddress address("127.0.0.1", 9000);
        auto client = transport.Connect(address);

        First::Proxy proxy1(client);

        // TODO: check connection status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server1Service1Connect1()
    {
        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);


        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        UT_AssertIsNotNull(server.get());

        // TODO: check bind status
        // TODO: check connection status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server2Service1Connect1()
    {
        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);


        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server1 = transport.Bind(serverAddress1, boost::ref(firstService));
        bond::comm::Server server2 = transport.Bind(serverAddress2, boost::ref(firstService));

        First::Proxy proxy1(transport.Connect(serverAddress1));
        First::Proxy proxy2(transport.Connect(serverAddress2));

        UT_AssertIsNotNull(server1.get());
        UT_AssertIsNotNull(server2.get());

        // TODO: check bind status
        // TODO: check connection status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server2Service2Connect1()
    {
        //
        // Create service.
        First_ServiceImpl firstService;
        boost::shared_ptr<Second_ServiceImpl> secondService(new Second_ServiceImpl());

        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);


        bond::comm::ServiceTable table;
        table.Using(bond::comm::FastWireProtocol())
             .Register(boost::ref(firstService))
             .Register(secondService);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server1 = transport.Bind(serverAddress1, table);
        bond::comm::Server server2 = transport.Bind(serverAddress2, table);

        auto client1 = transport.Connect(serverAddress1);
        auto client2 = transport.Connect(serverAddress2);

        First::Proxy proxy11(client1);
        First::Proxy proxy12(client2);

        Second::Proxy proxy21(client1);
        Second::Proxy proxy22(client2);

        UT_AssertIsNotNull(server1.get());
        UT_AssertIsNotNull(server2.get());

        // TODO: check bind status
        // TODO: check connection status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server2Service2Connect2()
    {
        //
        // Create service.
        First_ServiceImpl firstService;
        boost::shared_ptr<Second_ServiceImpl> secondService(new Second_ServiceImpl());

        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);


        bond::comm::ServiceTable table;
        table.Using(bond::comm::FastWireProtocol())
             .Register(boost::ref(firstService))
             .Register(secondService);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server1 = transport.Bind(serverAddress1, table);
        bond::comm::Server server2 = transport.Bind(serverAddress2, table);

        auto client11 = transport.Connect(serverAddress1);
        auto client21 = transport.Connect(serverAddress1);
        auto client12 = transport.Connect(serverAddress2);
        auto client22 = transport.Connect(serverAddress2);

        First::Proxy proxy11(client11);
        First::Proxy proxy12(client12);

        Second::Proxy proxy21(client21);
        Second::Proxy proxy22(client22);

        UT_AssertIsNotNull(server1.get());
        UT_AssertIsNotNull(server2.get());


        // TODO: check bind status
        // TODO: check connection status
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    static
    void Server1Service1Send0()
    {
        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        bond::comm::ServiceTable table;
        table.Register(boost::ref(firstService), bond::comm::FastWireProtocol());

        //
        // Start transport.
        //
        Transport transport;

        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send0")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            UT_AssertThrows((syncMessage.Deserialize(result, 500)), bond::Exception);
        }
    }

    static
    void Server1Service1Send1SocketAddress()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send1SocketAddress")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
        }
    }


    static
    void Server1Service1Send1Reconnect()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method, drop service, publish a new one,
        // test proxy instance.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send1Reconnect")
        {
            Params params;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            {
                Result result;
                syncMessage.Deserialize(result);
                UT_AssertIsTrue(result.z == 1);
            }

            server.reset();
            std::this_thread::sleep_for(std::chrono::milliseconds(1000));

            server = transport.Bind(address, boost::ref(firstService));

            proxy.Method1(params, syncMessage);

            {
                Result result;
                syncMessage.Deserialize(result);
                UT_AssertIsTrue(result.z == 1);
            }
        }


    }

    /*
    static
    void Server1Service1Send1UrlAddress()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        bond::UrlAddress address("localhost:9000");

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send1")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
        }
    }
    */

    static
    void Server2Service1Send1()
    {
        //
        // publish First_ServiceImpl at ports 9000, 9001
        // connect one First::Proxy instance to 9000 and
        // another to 9001 via independend connections.
        // invoke methods via both instances.
        //

        //
        // Create service.
        //
        First_ServiceImpl firstService;
        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);


        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server1 = transport.Bind(serverAddress1, boost::ref(firstService));
        bond::comm::Server server2 = transport.Bind(serverAddress2, boost::ref(firstService));

        First::Proxy proxy1(transport.Connect(serverAddress1));
        First::Proxy proxy2(transport.Connect(serverAddress2));

        LOOP("Server2Service1Send1")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy1.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);

            proxy2.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);
        }
    }

    static
    void Server2Service2Send1()
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
        boost::shared_ptr<Second_ServiceImpl> secondService(new Second_ServiceImpl());

        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);


        bond::comm::ServiceTable table;
        table.Using(bond::comm::FastWireProtocol())
             .Register(boost::ref(firstService))
             .Register(secondService);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server1 = transport.Bind(serverAddress1, table);
        bond::comm::Server server2 = transport.Bind(serverAddress2, table);

        auto client1 = transport.Connect(serverAddress1);
        auto client2 = transport.Connect(serverAddress2);

        First::Proxy proxy11(client1);
        First::Proxy proxy12(client2);

        Second::Proxy proxy21(client1);
        Second::Proxy proxy22(client2);

        LOOP("Server2Service1Send1")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy11.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);

            proxy12.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);

            proxy21.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 2);

            proxy22.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 2);
        }
    }

    static
    void Server2Service2Send2()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect First::Proxy and Second::Proxy to port 9000 and to 9001 via four independend connections.
        // invoke methods via all four proxies.
        //

        //
        // Create service.
        First_ServiceImpl firstService;
        boost::shared_ptr<Second_ServiceImpl> secondService(new Second_ServiceImpl());

        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);


        bond::comm::ServiceTable table;
        table.Using(bond::comm::FastWireProtocol())
             .Register(boost::ref(firstService))
             .Register(secondService);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server1 = transport.Bind(serverAddress1, table);
        bond::comm::Server server2 = transport.Bind(serverAddress2, table);

        auto client11 = transport.Connect(serverAddress1);
        auto client21 = transport.Connect(serverAddress1);
        auto client12 = transport.Connect(serverAddress2);
        auto client22 = transport.Connect(serverAddress2);

        First::Proxy proxy11(client11);
        First::Proxy proxy12(client12);

        Second::Proxy proxy21(client21);
        Second::Proxy proxy22(client22);

        LOOP("Server2Service2Send2")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy11.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);

            proxy12.Method1(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);

            proxy21.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 2);

            proxy22.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 2);
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

        suite.AddTestCase(Server1Service1StartStop, "Server1Service1StartStop");

        suite.AddTestCase(Server1Service2StartStop, "Server1Service2StartStop");

        suite.AddTestCase(Server2Service1StartStop, "Server2Service1StartStop");

        suite.AddTestCase(Server1Service1Connect0, "Server1Service1Connect0");

        suite.AddTestCase(Server1Service1Connect1, "Server1Service1Connect1");

        suite.AddTestCase(Server2Service1Connect1, "Server2Service1Connect1");

        suite.AddTestCase(Server2Service2Connect1, "Server2Service2Connect1");

        suite.AddTestCase(Server2Service2Connect2, "Server2Service2Connect2");

        suite.AddTestCase(Server1Service1Send0, "Server1Service1Send0");

        // TODO: add support of DNS resolver
        //suite.AddTestCase(Server1Service1Send1UrlAddress, "Server1Service1Send1UrlAddress");

        suite.AddTestCase(Server1Service1Send1SocketAddress, "Server1Service1Send1SocketAddress");

        //suite.AddTestCase(Server1Service1Send1Reconnect, "Server1Service1Send1Reconnect");

        suite.AddTestCase(Server2Service1Send1, "Server2Service1Send1");

        suite.AddTestCase(Server2Service2Send1, "Server2Service2Send1");

        suite.AddTestCase(Server2Service2Send2, "Server2Service2Send2");
    }
};

void init_unit_test_comm_basic()
{
    InitializeTests<BasicTransportTests, bond::comm::FastWireProtocol>();
}
