
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <unit_test_comm_void_reflection.h>
#include <unit_test_comm_void_comm.h>

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
class VoidTransportTests
{
    typedef T Transport;

    class Void_ServiceImpl
        : public VoidService
    {
        void MethodResultVoid(const bond::comm::payload<void>& /* input */,
                              const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Result r;
            r.z = 1;
            callback(boost::cref(r));
        }

        void MethodVoid(const bond::comm::payload<void>& /* input */,
                        const std::function<void (const bond::comm::message<void>&)>& callback)
        override
        {
            callback(bond::comm::message<void>());
        }

        void MethodVoidParams(const bond::comm::payload<Params>& /* input */,
                              const std::function<void (const bond::comm::message<void>&)>& callback)
        override
        {
            callback(bond::comm::message<void>());
        }

        void MethodResultVoidException(const bond::comm::payload<void>& /* input */,
                                       const std::function<void (const bond::comm::message<Result>&)>& /* callback */)
        override
        {
            throw std::runtime_error("MethodResultVoidException");
        }

        void MethodVoidException(const bond::comm::payload<void>& /* input */,
                                 const std::function<void (const bond::comm::message<void>&)>& /* callback */)
        override
        {
            throw std::runtime_error("MethodVoidException");
        }

        void MethodVoidExceptionParams(const bond::comm::payload<Params>& /* input */,
                                       const std::function<void (const bond::comm::message<void>&)>& /* callback */)
        override
        {
            throw std::runtime_error("MethodVoidExceptionParams");
        }

    public:

        Void_ServiceImpl()
        {}
    };



    static
    void Server1Service1Void1()
    {
        //
        // publish Void_ServiceImpl at port 9000
        // connect one VoidService::Proxy instance to it.
        // invoke method with void response.
        //

        //
        // Create service.
        Void_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Void_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        VoidService::Proxy proxy(client);

        LOOP("Server1Service1Void1")
        {
            WaitForResponse<Result> syncMessage;
            proxy.MethodResultVoid(syncMessage);

            Result result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 1);
        }
    }

    static
    void Server1Service1Void2()
    {
        //
        // publish Void_ServiceImpl at port 9000
        // connect one VoidService::Proxy instance to it.
        // invoke method with void request.
        //

        //
        // Create service.
        Void_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Void_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        VoidService::Proxy proxy(client);

        LOOP("Server1Service1Void2")
        {
            Params params;

            WaitForResponse<void> syncMessage;
            proxy.MethodVoidParams(params, syncMessage);

            syncMessage.Wait();
        }
    }

    static
    void Server1Service1Void3()
    {
        //
        // publish Void_ServiceImpl at port 9000
        // connect one VoidService::Proxy instance to it.
        // invoke method with void request and void response.
        //

        //
        // Create service.
        Void_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Void_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        VoidService::Proxy proxy(client);

        LOOP("Server1Service1Void3")
        {
            WaitForResponse<void> syncMessage;
            proxy.MethodVoid(syncMessage);

            syncMessage.Wait();
        }
    }


    static
    void Server1Service1VoidException1()
    {
        //
        // publish Void_ServiceImpl at port 9000
        // connect one VoidService::Proxy instance to it.
        //
        // invoke method that throws exception
        // with void response.
        //

        //
        // Create service.
        Void_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Void_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        VoidService::Proxy proxy(client);

        LOOP("Server1Service1VoidException1")
        {
            WaitForResponse<Result> syncMessage;
            proxy.MethodResultVoidException(syncMessage);

            Result result;
            UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);
        }
    }

    static
    void Server1Service1VoidException2()
    {
        //
        // publish Void_ServiceImpl at port 9000
        // connect one VoidService::Proxy instance to it.
        //
        // invoke method that throws exception
        // with void request.
        //

        //
        // Create service.
        Void_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Void_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        VoidService::Proxy proxy(client);

        LOOP("Server1Service1VoidException2")
        {
            Params params;

            WaitForResponse<void> syncMessage;
            proxy.MethodVoidExceptionParams(params, syncMessage);

            UT_AssertThrows((syncMessage.Wait()), bond::Exception);
        }
    }

    static
    void Server1Service1VoidException3()
    {
        //
        // publish Void_ServiceImpl at port 9000
        // connect one VoidService::Proxy instance to it.
        //
        // invoke method that throws exception
        // with void request and void response.
        //

        //
        // Create service.
        Void_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Void_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        VoidService::Proxy proxy(client);

        LOOP("Server1Service1VoidException3")
        {
            WaitForResponse<void> syncMessage;
            proxy.MethodVoidException(syncMessage);

            UT_AssertThrows((syncMessage.Wait()), bond::Exception);
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

        suite.AddTestCase(Server1Service1Void1, "Server1Service1Void1");

        suite.AddTestCase(Server1Service1Void2, "Server1Service1Void2");

        suite.AddTestCase(Server1Service1Void3, "Server1Service1Void3");

        suite.AddTestCase(Server1Service1VoidException1, "Server1Service1VoidException1");

        suite.AddTestCase(Server1Service1VoidException2, "Server1Service1VoidException2");

        suite.AddTestCase(Server1Service1VoidException3, "Server1Service1VoidException3");
    }
};


void init_unit_test_comm_void()
{
    InitializeTests<VoidTransportTests, bond::comm::FastWireProtocol>();
}
