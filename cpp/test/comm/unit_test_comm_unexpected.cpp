
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
class UnexpectedTransportTests
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
            m_method1StartEvent.Notify();

            Params p;
            input.value().Deserialize(p);

            switch (p.op)
            {
                //
                // Double dispatch.
                //
                case DoubleDispatch:
                {
                    {
                        Result r;
                        r.z = 4;
                        callback(boost::cref(r));

                        try
                        {
                            r.z = 5;
                            callback(boost::cref(r));

                            BOOST_ASSERT(false);
                        }
                        catch (const bond::comm::InvalidInvocationException&)
                        {
                        }

                        try
                        {
                            r.z = 6;
                            callback(boost::cref(r));

                            BOOST_ASSERT(false);
                        }
                        catch (const bond::comm::InvalidInvocationException&)
                        {
                        }
                    }

                    return;
                }

                case DelayCallback:
                    {
                        Result r;
                        r.z = GetMilliseconds();

                        ++m_method1SemaphoreCount;

                        return m_threads.schedule(
                            boost::bind(&First_ServiceImpl::DelayedInvoke,
                                        this,
                                        callback,
                                        r,
                                        true));
                    }

                default:
                {
                    UT_AssertIsTrue(false);
                }
            }
        }

    public:

        First_ServiceImpl()
            : m_threads(1)
            , m_method1SemaphoreCount()
        {}

        void SignalDelayed()
        {
            m_method1Delay.Notify();
        }

        void WaitForMethod1Start()
        {
            m_method1StartEvent.Wait();
        }

        void WaitForMethod1Complete()
        {
            ++m_method1SemaphoreCount;
            while (--m_method1SemaphoreCount > 0)
            {
                m_method1Semaphore.Wait();
            }
        }

    private:

        void DelayedInvoke(const std::function<void (const bond::comm::message<Result>&)>& callback,
                           Result result,
                           bool delay)
        {
            // First wait for at least 1 call start.
            if (delay)
            {
                m_method1Delay.Wait();

                // adjust result.
                result.z = GetMillisecondsDiff(result.z, GetMilliseconds());
            }

            callback(boost::cref(result));

            m_method1Semaphore.Notify();
        }

        bond::comm::thread_pool m_threads;

        Semaphore m_method1Delay;

        Semaphore m_method1StartEvent;

        Semaphore m_method1Semaphore;

        std::atomic<std::uint32_t> m_method1SemaphoreCount;
    };


    static
    void Server1Service1DoubleCallback1()
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
        bond::comm::SocketAddress address("127.0.0.1", 9000);
        bond::comm::Server server = transport.Bind(address, boost::ref(service));

        First::Proxy proxy(transport.Connect(address));

        LOOP("Server1Service1DoubleCallback")
        {
            Params params;
            Result result;

            params.op = DoubleDispatch;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 4);
        }
    }

    static
    void Server1Service1DropException1()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        //
        // invoke method, shutdown service while in progress.
        //

        LOOP("Server1Service1DropException1")
        {

            WaitForResponse<Result> syncMessage;


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

            {
                auto client = transport.Connect(address);

                First::Proxy proxy(client);

                Params params;
                params.op = DelayCallback; // Sleep at server side until explicitly signaled

                proxy.Method1(params, syncMessage);
                firstService.WaitForMethod1Start(); // Wait for Method1 to start

                //
                // Client will be destroyed here.
                //
            }

            Result result;

            UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);

            firstService.SignalDelayed(); // Signal server side delayed thread to wake up

            firstService.WaitForMethod1Complete(); // Wait for delayed thread to respond
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

        //suite.AddTestCase(Server1Service1DoubleCallback1, "Server1Service1DoubleCallback1");

        suite.AddTestCase(Server1Service1DropException1, "Server1Service1DropException1");
    }
};


void init_unit_test_comm_unexpected()
{
    InitializeTests<UnexpectedTransportTests, bond::comm::FastWireProtocol>();
}
