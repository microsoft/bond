#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <generic_reflection.h>
#include <generic_comm.h>

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
class GenericServiceTransportTests
{
    typedef T Transport;

    class GenericServiceImpl
        : public Generic<unittest::comm::Data>
    {
        void MethodGenParams(const bond::comm::payload<Params>& input,
                             const std::function<void (const bond::comm::message<Data>&)>& callback)
        override
        {
            unittest::comm::Params p;
            input.value().Deserialize(p);

            unittest::comm::Data data;
            data.m_x = p.x;
            callback(boost::cref(data));
        }

        void MethodResultGen(const bond::comm::payload<Data>& input,
                             const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            unittest::comm::Data p;
            input.value().Deserialize(p);

            unittest::comm::Result result;
            result.z = static_cast<uint32_t>(p.m_x);
            callback(boost::cref(result));
        }

        void MethodGen(const bond::comm::payload<Data>& input,
                       const std::function<void (const bond::comm::message<Data>&)>& callback)
        override
        {
            unittest::comm::Data p;
            input.value().Deserialize(p);
            callback(boost::cref(p));
        }

        void MethodBox(const bond::comm::payload<bond::Box<Data>>& input,
                       const std::function<void (const bond::comm::message<bond::Box<Data>>&)>& callback)
        override
        {
            bond::Box<unittest::comm::Data> p;
            input.value().Deserialize(p);
            callback(boost::cref(p));
        }
    };


    static
    void Server1Service1GenericService1()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic response.
        //

        //
        // Create service.
        GenericServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        Generic<unittest::comm::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService1")
        {
            unittest::comm::Data data;
            data.m_x = 2;

            WaitForResponse<unittest::comm::Result> syncMessage;
            proxy.MethodResultGen(boost::cref(data), syncMessage);

            unittest::comm::Result result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == data.m_x);
        }
    }

    static
    void Server1Service1GenericService2()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic request.
        //

        //
        // Create service.
        GenericServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        Generic<unittest::comm::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService2")
        {
            unittest::comm::Params params;
            params.x = 2;

            WaitForResponse<unittest::comm::Data> syncMessage;
            proxy.MethodGenParams(params, syncMessage);

            unittest::comm::Data result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.m_x == params.x);
        }
    }

    static
    void Server1Service1GenericService3()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic request and void response.
        //

        //
        // Create service.
        GenericServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        Generic<unittest::comm::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService3")
        {
            unittest::comm::Data data;
            data.m_x = 2;

            WaitForResponse<unittest::comm::Data> syncMessage;
            proxy.MethodGen(boost::cref(data), syncMessage);

            unittest::comm::Data result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.m_x == data.m_x);
        }
    }

    static
    void Server1Service1GenericService4()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic request and void response.
        //

        //
        // Create service.
        GenericServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        Generic<unittest::comm::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService4")
        {
            bond::Box<unittest::comm::Data> params;
            params.value.m_x = 2;

            WaitForResponse<bond::Box<unittest::comm::Data>> syncMessage;
            proxy.MethodBox(params, syncMessage);

            bond::Box<unittest::comm::Data> result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.value.m_x == params.value.m_x);
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

        suite.AddTestCase(Server1Service1GenericService1, "Server1Service1GenericService1");

        suite.AddTestCase(Server1Service1GenericService2, "Server1Service1GenericService2");

        suite.AddTestCase(Server1Service1GenericService3, "Server1Service1GenericService3");

        suite.AddTestCase(Server1Service1GenericService4, "Server1Service1GenericService4");;
    }
};


bool init_unit_test()
{
    InitializeTests<GenericServiceTransportTests, bond::comm::FastWireProtocol>();
    return true;
}
