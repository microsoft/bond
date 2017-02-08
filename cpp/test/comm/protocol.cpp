
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
class ProtocolTransportTests
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


    static
    void Server1Service1Protocol1()
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

        LOOP("Server1Service1Protocol1")
        {
            First::Proxy proxy1(client);

            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy1.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
            result.z = 0;

            First::Proxy proxy2(client);

            proxy2.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
        }
    }

    static
    void Server1Service1Protocol2()
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

        auto client1 = transport.Connect(address);
        auto client2 = transport.Connect(address, bond::comm::CompactWireProtocol());

        LOOP("Server1Service1Protocol2")
        {
            First::Proxy proxy1(client1);

            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy1.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
            result.z = 0;

            First::Proxy proxy2(client2);

            proxy2.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
        }
    }


    static
    void Server1Service1Protocol3()
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

        const bond::comm::FastWireProtocol fastWireProtocol;
        const bond::comm::CompactWireProtocol compactWireProtocol;

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client1 = transport.Connect(address, fastWireProtocol);
        auto client2 = transport.Connect(address, compactWireProtocol);

        LOOP("Server1Service1Protocol3")
        {
            First::Proxy proxy1(client1);

            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy1.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
            result.z = 0;

            First::Proxy proxy2(client2);

            proxy2.Method1(params, syncMessage);

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

        suite.AddTestCase(Server1Service1Protocol1, "Server1Service1Protocol1");

        suite.AddTestCase(Server1Service1Protocol2, "Server1Service1Protocol2");

        suite.AddTestCase(Server1Service1Protocol3, "Server1Service1Protocol3");
    }
};

void init_protocol()
{
    InitializeTests<ProtocolTransportTests, bond::comm::FastWireProtocol>();
}
