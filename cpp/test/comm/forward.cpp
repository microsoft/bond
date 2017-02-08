
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <comm_test_common_reflection.h>
#include <comm_test_common_comm.h>

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
class ForwardTransportTests
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

            //
            // Forward to child.
            //
            m_proxy.Method1(input, callback);
        }

        //
        // Child proxy.
        //
        First::Proxy& m_proxy;

    public:
        Second_ServiceImpl(First::Proxy& proxy)
            : m_proxy(proxy)
        {}
    };

    static
    void Server2Service2Forward1()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect Second_ServiceImpl to First_ServiceImpl via First::Proxy and
        // forward requests/responses with contexts. Both legs use fast protocol.
        //

        //
        // Create service.
        First_ServiceImpl firstService;

        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server1 = transport.Bind(serverAddress1, boost::ref(firstService));
        First::Proxy proxy1(transport.Connect(serverAddress1));

        boost::shared_ptr<Second> secondService(new Second_ServiceImpl(proxy1));

        bond::comm::Server server2 = transport.Bind(serverAddress2, secondService);
        Second::Proxy proxy2(transport.Connect(serverAddress2));

        LOOP("Server2Service2Forward1")
        {
            Params params;
            Result result;

            WaitForResponse<Result> syncMessage;
            proxy2.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);
        }
    }

    static
    void Server2Service2Forward2()
    {
        //
        // publish both First_ServiceImpl and Second_ServiceImpl at ports 9000, 9001
        // connect Second_ServiceImpl to First_ServiceImpl via First::Proxy and
        // forward requests/responses with contexts. 1st leg uses compact protocol, the 2nd - fast.
        //

        //
        // Create service.
        First_ServiceImpl firstService;

        bond::comm::SocketAddress serverAddress1("127.0.0.1", 9000),
                            serverAddress2("127.0.0.1", 9001);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server1 = transport.Bind(serverAddress1, boost::ref(firstService));
        First::Proxy proxy1(transport.Connect(serverAddress1));

        boost::shared_ptr<Second> secondService(new Second_ServiceImpl(proxy1));

        bond::comm::Server server2 = transport.Bind(serverAddress2, secondService);
        Second::Proxy proxy2(transport.Connect(serverAddress2, bond::comm::CompactWireProtocol()));

        LOOP("Server2Service2Forward2")
        {
            Params params;
            Result result;

            WaitForResponse<Result> syncMessage;
            proxy2.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);
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

        suite.AddTestCase(Server2Service2Forward1, "Server2Service2Forward1");

        suite.AddTestCase(Server2Service2Forward2, "Server2Service2Forward2");
    }
};


void init_forward()
{
    InitializeTests<ForwardTransportTests, bond::comm::FastWireProtocol>();
}
