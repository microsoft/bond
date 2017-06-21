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
class GenericTypeTransportTests
{
    typedef T Transport;

    class GenericType_ServiceImpl
        : public GenericType
    {
        void MethodGenParams(const bond::comm::payload<Params>& input,
                             const std::function<void (const bond::comm::message<bond::Box<Result>>&)>& callback)
        override
        {
            Params p;
            input.value().Deserialize(p);

            bond::Box<Result> r;
            r.value.z = p.x;
            callback(boost::cref(r));
        }

        void MethodResultGen(const bond::comm::payload<bond::Box<Params>>& input,
                             const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            bond::Box<Params> p;
            input.value().Deserialize(p);

            Result r;
            r.z = p.value.x;
            callback(boost::cref(r));
        }

        void MethodGen(const bond::comm::payload<bond::Box<Params>>& input,
                       const std::function<void (const bond::comm::message<bond::Box<Result>>&)>& callback)
        override
        {
            bond::Box<Params> p;
            input.value().Deserialize(p);

            bond::Box<Result> r;
            r.value.z = p.value.x;
            callback(boost::cref(r));
        }

    public:

        GenericType_ServiceImpl()
        {}
    };


    static
    void Server1Service1GenericType1()
    {
        //
        // publish GenericType_ServiceImpl at port TEST_PORT_1
        // connect one GenericType::Proxy instance to it.
        // invoke method with generic response.
        //

        //
        // Create service.
        GenericType_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);

        //
        // Start transport.
        //
        Transport transport;

        // Publish GenericType_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        GenericType::Proxy proxy(client);

        LOOP("Server1Service1GenericType1")
        {
            bond::Box<Params> params;
            params.value.x = 2;

            WaitForResponse<Result> syncMessage;
            proxy.MethodResultGen(params, syncMessage);

            Result result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 2);
        }
    }

    static
    void Server1Service1GenericType2()
    {
        //
        // publish GenericType_ServiceImpl at port TEST_PORT_1
        // connect one GenericType::Proxy instance to it.
        // invoke method with generic request.
        //

        //
        // Create service.
        GenericType_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);

        //
        // Start transport.
        //
        Transport transport;

        // Publish GenericType_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        GenericType::Proxy proxy(client);

        LOOP("Server1Service1GenericType2")
        {
            Params params;
            params.x = 2;

            WaitForResponse<bond::Box<Result>> syncMessage;
            proxy.MethodGenParams(params, syncMessage);

            bond::Box<Result> result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.value.z == 2);
        }
    }

    static
    void Server1Service1GenericType3()
    {
        //
        // publish GenericType_ServiceImpl at port TEST_PORT_1
        // connect one GenericType::Proxy instance to it.
        // invoke method with generic request and void response.
        //

        //
        // Create service.
        GenericType_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);

        //
        // Start transport.
        //
        Transport transport;

        // Publish GenericType_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        GenericType::Proxy proxy(client);

        LOOP("Server1Service1GenericType3")
        {
            bond::Box<Params> params;
            params.value.x = 2;

            WaitForResponse<bond::Box<Result>> syncMessage;
            proxy.MethodGen(params, syncMessage);

            bond::Box<Result> result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.value.z == 2);
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

        suite.AddTestCase(Server1Service1GenericType1, "Server1Service1GenericType1");

        suite.AddTestCase(Server1Service1GenericType2, "Server1Service1GenericType2");

        suite.AddTestCase(Server1Service1GenericType3, "Server1Service1GenericType3");
    }
};


bool init_unit_test()
{
    InitializeTests<GenericTypeTransportTests, bond::comm::FastWireProtocol>();
    return true;
}
