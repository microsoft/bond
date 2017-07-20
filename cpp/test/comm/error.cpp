#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <comm_test_common_apply.h>
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

const static int InnerInnerErrorCode = 0x00009012;
const static int InnerErrorCode = 0x00001234;
const static int OuterErrorCode = 0x00005678;
const static std::string InnerInnerErrorMessage = "Inner Inner error";
const static std::string InnerErrorMessage = "Inner error";
const static std::string OuterErrorMessage = "Outer error";

template <typename T>
class ErrorTransportTests
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
                //
                // Throw exception.
                //
                case ThrowStdException:
                    throw std::runtime_error("ex p.op == ThrowStdException");

                case ReturnUserError:
                {
                    bond::comm::Error outerError, innerError, innerInnerError;
                    innerInnerError.error_code = InnerInnerErrorCode;
                    innerInnerError.message = InnerInnerErrorMessage;
                    innerError.error_code = InnerErrorCode;
                    innerError.message = InnerErrorMessage;
                    innerError.inner_error.set(bond::bonded<bond::comm::Error>(innerInnerError));
                    outerError.error_code = OuterErrorCode;
                    outerError.message = OuterErrorMessage;
                    outerError.inner_error.set(bond::bonded<bond::comm::Error>(innerError));
                    callback(bond::comm::error(outerError));
                    return;
                }

                //
                // Forget callback (would raise an exception).
                //
                case ForgetCallback: return;

                default: UT_AssertIsTrue(false);
            }
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
    };

    static
    void Server1Service1UserError1()
    {
        //
        // user error sent by serice implementation
        // connect First::Proxy to First_Service
        //

        //
        // Create service.
        First_ServiceImpl firstService;

        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);


        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1UserError1")
        {
            Params params;
            Result result;

            params.op = ReturnUserError;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            bond::comm::message<Result> message = syncMessage.GetResult();

            UT_AssertIsTrue(message.is_error());

            const bond::comm::Error &outerError = message.err();
            UT_AssertAreEqual(OuterErrorCode, outerError.error_code);
            UT_AssertAreEqual(OuterErrorMessage, outerError.message);
            UT_AssertIsTrue(outerError.inner_error.hasvalue());

            const bond::comm::Error &innerError = outerError.inner_error.value().Deserialize();
            UT_AssertAreEqual(InnerErrorCode, innerError.error_code);
            UT_AssertAreEqual(InnerErrorMessage, innerError.message);
            UT_AssertIsTrue(innerError.inner_error.hasvalue());

            const bond::comm::Error &innerInnerError = innerError.inner_error.value().Deserialize();
            UT_AssertAreEqual(InnerInnerErrorCode, innerInnerError.error_code);
            UT_AssertAreEqual(InnerInnerErrorMessage, innerInnerError.message);
        }
    }

    static
    void Server1Service1Exception1()
    {
        //
        // exception raised by serice implementation
        // connect First::Proxy to First_Service
        //

        //
        // Create service.
        First_ServiceImpl firstService;

        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);


        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));
        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Exception1")
        {
            Params params;
            Result result;

            params.op = ThrowStdException;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            bond::comm::message<Result> message = syncMessage.GetResult();

            UT_AssertIsTrue(message.is_error());
            const bond::comm::Error &outerError = message.err();
            UT_AssertAreEqual(bond::comm::ErrorCode::INTERNAL_SERVER_ERROR, outerError.error_code);
            UT_AssertAreEqual("", outerError.message);
        }
    }

    static
    void Server1Service1Exception2()
    {
        //
        // exception raised by calltable because of unknown service
        // connect First::Proxy to Second_Service
        //

        //
        // Create service.
        Second_ServiceImpl secondService;

        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);


        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service twice.
        bond::comm::Server server = transport.Bind(address, boost::ref(secondService));
        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Exception2")
        {
            Params params;
            Result result;

            params.op = ThrowStdException;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);
        }
    }

    static
    void Server1Service1Exception3()
    {
        //
        // exception raised by forgotten callback
        //

        // Create service.
        //
        First_ServiceImpl service;

        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        //
        bond::comm::SocketAddress address("127.0.0.1", TEST_PORT_1);
        bond::comm::Server server = transport.Bind(address, boost::ref(service));

        First::Proxy proxy(transport.Connect(address));

        LOOP("Server1Service1Exception3")
        {
            Params params;
            Result result;

            params.op = ForgetCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);
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

        suite.AddTestCase(Server1Service1UserError1, "Server1Service1UserError1");

        suite.AddTestCase(Server1Service1Exception1, "Server1Service1Exception1");

        suite.AddTestCase(Server1Service1Exception2, "Server1Service1Exception2");

        suite.AddTestCase(Server1Service1Exception3, "Server1Service1Exception3");

    }
};


bool init_unit_test()
{
    InitializeTests<ErrorTransportTests, bond::comm::FastWireProtocol>();
    return true;
}
