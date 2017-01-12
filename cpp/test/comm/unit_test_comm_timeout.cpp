
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
class TimeoutTransportTests
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
                // Fall down to regular sync callback.
                //
                case SyncCallback:
                    {
                        //
                        // Return r.z = 1 immediately.
                        //
                        Result r;
                        r.z = 1;
                        return callback(boost::cref(r));
                    }

                case AsyncCallback:
                    {
                        Result r;
                        r.z = 3;

                        ++m_method1SemaphoreCount;

                        return m_threads.schedule(
                            boost::bind(&First_ServiceImpl::DelayedInvoke,
                                        this,
                                        callback,
                                        r,
                                        false));
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

        explicit
        First_ServiceImpl(uint32_t instance)
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
    void Server1Service1Timeout1()
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

        LOOP("Server1Service1Timeout1")
        {
            {
                Params params;
                Result result;

                params.op = DelayCallback; // Sleep at server side until explicitly signaled

                WaitForResponse<Result> syncMessage;
                proxy.Method1(params, syncMessage);

                firstService.WaitForMethod1Start(); // Wait for Method1 to start

                UT_AssertThrows((syncMessage.Deserialize(result, 50)), bond::Exception);

                firstService.SignalDelayed(); // Signal server side delayed thread to wake up
            }
            firstService.WaitForMethod1Complete(); // Wait for delayed thread to respond
        }
    }

    static
    void Server1Service1Timeout2()
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
        // Timer dispatch threads.
        //
        bond::comm::thread_pool threads;

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        First::Proxy proxy(client);

        LOOP("Server1Service1Timeout2")
        {
            {
                //
                // Response shall come first.
                //

                Params params;
                Result result;

                params.op = SyncCallback; // Respond at server side immediately.

                WaitForResponse<Result> syncMessage;
                WaitForResponse<Result> lateMessage;
                proxy.Method1(params,
                              bond::comm::schedule_timeout<Result>(threads.get_io_service(), syncMessage, MAX_WAIT_IN_MSSECONDS, lateMessage));

                syncMessage.Deserialize(result); // Shouldn't throw.
            }

            {
                //
                // Wait for response after timeout.
                //

                Params params;
                Result result;

                params.op = DelayCallback; // Sleep at server side until explicitly signaled

                WaitForResponse<Result> syncMessage;
                WaitForResponse<Result> lateMessage;
                proxy.Method1(params,
                              bond::comm::schedule_timeout<Result>(threads.get_io_service(), syncMessage, 10, lateMessage));

                firstService.WaitForMethod1Start(); // Wait for Method1 to start

                UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);

                firstService.SignalDelayed(); // Signal server side delayed thread to wake up

                lateMessage.Deserialize(result);
            }
            firstService.WaitForMethod1Complete(); // Wait for delayed thread to respond

            {
                //
                // Drop response.
                //

                Params params;
                Result result;

                params.op = DelayCallback; // Sleep at server side until explicitly signaled

                WaitForResponse<Result> syncMessage;
                WaitForResponse<Result> lateMessage;
                proxy.Method1(params,
                              bond::comm::schedule_timeout<Result>(threads.get_io_service(), syncMessage, 10, lateMessage));

                firstService.WaitForMethod1Start(); // Wait for Method1 to start

                UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);

                firstService.SignalDelayed(); // Signal server side delayed thread to wake up
            }
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

        suite.AddTestCase(Server1Service1Timeout1, "Server1Service1Timeout1");

        suite.AddTestCase(Server1Service1Timeout2, "Server1Service1Timeout2");

    }
};


void init_unit_test_comm_timeout()
{
    InitializeTests<TimeoutTransportTests, bond::comm::FastWireProtocol>();
}
