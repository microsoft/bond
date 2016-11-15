
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

static const uint32_t ITERATIONS = 2;

namespace bond { namespace comm
{

void LogHandler(const char* functionName,
                const char* fileName,
                uint32_t lineNumber,
                LogSeverity /*severity*/,
                const char* category,
                const char* message)
{
#if defined(_MSC_VER)

    OutputDebugStringA((bond::detail::string_stream() <<
        category << ", "<< message <<
        " [" << functionName << " " << fileName << "(" << lineNumber << ") " << "]\n"
    ).content());

#else

    //bond::detail::string_stream ss;
    //ss << category << ", "
    //   << message << " ["
    //   << functionName << " "
    //   << fileName << "("
    //   << lineNumber << ") ]\n";
    //puts(ss.content());
    //BOOST_TEST_MESSAGE(ss.content());

#endif
}
} } // namespace bond.comm

using namespace unittest::rpc;

const static int InnerInnerErrorCode = 0x00009012;
const static int InnerErrorCode = 0x00001234;
const static int OuterErrorCode = 0x00005678;
const static std::string InnerInnerErrorMessage = "Inner Inner error";
const static std::string InnerErrorMessage = "Inner error";
const static std::string OuterErrorMessage = "Outer error";

template <typename T>
class GenericTransportTests
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

                case ForwardCallback:
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

                //
                // Return r.z == 2 after 1s delay.
                //
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
                    break;
                }
            }
        }

        void MethodResultVoid(const bond::comm::payload<void>& /* input */,
                              const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Result r;
            r.z = 1;
            callback(boost::cref(r));
        }

        void GetInstance(const bond::comm::payload<void>& /* input */,
                         const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Result r;
            r.z = m_instance;
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

        void Event(const bond::comm::payload<Params>& event)
        override
        {
            Params p;
            event.value().Deserialize(p);
            m_eventParam = p.x;

            m_eventInvoke.Notify();
        }

    public:

        First_ServiceImpl()
            : m_threads(1)
            , m_method1SemaphoreCount()
            , m_instance()
            , m_eventParam()
        {}

        explicit
        First_ServiceImpl(uint32_t instance)
            : m_threads(1)
            , m_method1SemaphoreCount()
            , m_instance(instance)
            , m_eventParam()
        {}


        void SignalDelayed()
        {
            m_method1Delay.Notify();
        }

        void WaitForMethod1Start()
        {
            // First wait for at least 1 call start.
            m_method1StartEvent.Wait();
        }

        void WaitForMethod1Complete()
        {
            //WaitForMethod1Start();
            SignalDelayed();

            ++m_method1SemaphoreCount;
            while (--m_method1SemaphoreCount > 0)
            {
                m_method1Semaphore.Wait();
            }
        }

        uint32_t GetEventParam() const
        {
            m_eventInvoke.Wait();
            return m_eventParam;
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

        mutable Semaphore m_eventInvoke;

        Semaphore m_method1Delay;

        Semaphore m_method1StartEvent;

        Semaphore m_method1Semaphore;

        std::atomic<std::uint32_t> m_method1SemaphoreCount;

        uint32_t m_instance;

        uint32_t m_eventParam;

        boost::shared_ptr<::unittest::rpc::Listener> m_service;
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

            switch (p.op)
            {
                //
                // Throw exception.
                //
                case SyncCallback:
                {
                    Result r;
                    r.z = 2;
                    callback(boost::cref(r));

                    break;
                }

                //
                // Forward to child.
                //
                case ForwardCallback:
                {
                    m_proxy->Method1(input, callback);
                    return;
                }

                default:
                {
                    break;
                }
            }
        }

        //
        // Child proxy.
        //
        First::Proxy* m_proxy;

    public:
        Second_ServiceImpl(First::Proxy* proxy)
            : m_proxy(proxy)
        {}

        Second_ServiceImpl()
            : m_proxy(NULL)
        {}
    };

    class GenericServiceImpl
        : public Generic<unittest::rpc::Data>
    {
        void MethodGenParams(const bond::comm::payload<Params>& input,
                             const std::function<void (const bond::comm::message<Data>&)>& callback)
        override
        {
            unittest::rpc::Params p;
            input.value().Deserialize(p);

            unittest::rpc::Data data;
            data.m_x = p.x;
            callback(boost::cref(data));
        }

        void MethodResultGen(const bond::comm::payload<Data>& input,
                             const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            unittest::rpc::Data p;
            input.value().Deserialize(p);

            unittest::rpc::Result result;
            result.z = static_cast<uint32_t>(p.m_x);
            callback(boost::cref(result));
        }

        void MethodGen(const bond::comm::payload<Data>& input,
                       const std::function<void (const bond::comm::message<Data>&)>& callback)
        override
        {
            unittest::rpc::Data p;
            input.value().Deserialize(p);
            callback(boost::cref(p));
        }

        void MethodBox(const bond::comm::payload<bond::Box<Data>>& input,
                       const std::function<void (const bond::comm::message<bond::Box<Data>>&)>& callback)
        override
        {
            bond::Box<unittest::rpc::Data> p;
            input.value().Deserialize(p);
            callback(boost::cref(p));
        }
    };

    struct Test_ServiceImpl
        : public Test
    {
        void TestMethod1(const bond::comm::payload<Params>&,
                         const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Result r;
            callback(boost::cref(r));
        }

        void TestMethod2(const bond::comm::payload<void>&,
                         const std::function<void (const bond::comm::message<void>&)>& callback)
        override
        {
            callback(bond::comm::message<void>());
        }

    public:

        Test_ServiceImpl()
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

                params.op = DelayCallback;

                WaitForResponse<Result> syncMessage;
                proxy.Method1(params, syncMessage);
                firstService.WaitForMethod1Start();

                UT_AssertThrows((syncMessage.Deserialize(result, 100)), bond::Exception);

            }
            firstService.WaitForMethod1Complete(); // Wait for delayed messages processed.
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

                params.op = DelayCallback; // Sleep at server side for 1000ms.

                WaitForResponse<Result> syncMessage;
                WaitForResponse<Result> lateMessage;
                proxy.Method1(params,
                              bond::comm::schedule_timeout<Result>(threads.get_io_service(), syncMessage, 10, lateMessage));

                UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);

                firstService.SignalDelayed();

                lateMessage.Deserialize(result);
            }

            {
                //
                // Drop response.
                //

                Params params;
                Result result;

                params.op = DelayCallback; // sleep at server side for 1000ms

                WaitForResponse<Result> syncMessage;
                WaitForResponse<Result> lateMessage;
                proxy.Method1(params,
                              bond::comm::schedule_timeout<Result>(threads.get_io_service(), syncMessage, 10, lateMessage));

                UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);
                firstService.SignalDelayed();
            }
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

    static
    void Server1Service1Send1LocalhostAny()
    {
        //
        // publish First_ServiceImpl at port 9000, 0.0.0.0
        // connect one First::Proxy instance to it via localhost.
        // invoke method.
        //

        //
        // Create service.
        First_ServiceImpl firstService;

        bond::comm::SocketAddress serverAddress(static_cast<uint32_t>(INADDR_ANY), 9000);
        bond::comm::SocketAddress clientAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish First_Service.
        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(clientAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Send1LocalhostAny")
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
    void Server1Service1Async1()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        //
        // Invoke method, which execution will be scheduled on server side.
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

        LOOP("Server1Service1Async1")
        {
            Params params;
            Result result;

            params.op = AsyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.z == 3);
        }
    }

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


    static
    void Server1Service1Event0()
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

        LOOP("Server1Service1Event0")
        {
            Params params;

            proxy.Event(params);
        }
    }


    static
    void Server1Service1Event1()
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

        LOOP("Server1Service1Event1")
        {
            Params params;
            params.x = 1;

            proxy.Event(params);

            UT_AssertIsTrue(firstService.GetEventParam() == params.x);
        }
    }


    static
    void Server1Service1Layer0()
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
        bond::comm::LayerTransport<Transport, bond::comm::CompactWireProtocol>  transport;

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);

        First::Proxy proxy(client);

        LOOP("Server1Service1Layer0")
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


    struct Layer1
    {
        void OnSend(bond::comm::MessageType payload_type,
                    const std::string& /*service_name*/,
                    const std::string& /*method_name*/,
                    Data&)
        {
            *_stream << "1:snd-";
            switch (payload_type)
            {
                case bond::comm::MessageType::EVENT: *_stream << "evt,"; break;
                case bond::comm::MessageType::REQUEST: *_stream << "req,"; break;
                case bond::comm::MessageType::RESPONSE: *_stream << "resp,"; break;
                default: *_stream << "undef,"; break;
            }
        }


        void OnReceive(bond::comm::MessageType payload_type,
                       const std::string& /*service_name*/,
                       const std::string& /*method_name*/,
                       Data&)
        {
            *_stream << "1:rcv-";
            switch (payload_type)
            {
                case bond::comm::MessageType::EVENT: *_stream << "evt."; break;
                case bond::comm::MessageType::REQUEST: *_stream << "req,"; break;
                case bond::comm::MessageType::RESPONSE: *_stream << "resp."; break;
                default: *_stream << "undef."; break;
            }
        }


        explicit
        Layer1(std::stringstream* ss = nullptr) : _stream(ss) {}

        std::stringstream* _stream;
    };


    struct Layer2
    {
        void OnSend(bond::comm::MessageType payload_type,
                    const std::string& /*service_name*/,
                    const std::string& /*method_name*/,
                    Data&)
        {
            *_stream << "2:snd-";
            switch (payload_type)
            {
                case bond::comm::MessageType::EVENT: *_stream << "evt,"; break;
                case bond::comm::MessageType::REQUEST: *_stream << "req,"; break;
                case bond::comm::MessageType::RESPONSE: *_stream << "resp,"; break;
                default: *_stream << "undef,"; break;
            }
        }


        void OnReceive(bond::comm::MessageType payload_type,
                       const std::string& /*service_name*/,
                       const std::string& /*method_name*/,
                       Data&)
        {
            *_stream << "2:rcv-";
            switch (payload_type)
            {
                case bond::comm::MessageType::EVENT: *_stream << "evt,"; break;
                case bond::comm::MessageType::REQUEST: *_stream << "req,"; break;
                case bond::comm::MessageType::RESPONSE: *_stream << "resp,"; break;
                default: *_stream << "undef,"; break;
            }
        }


        explicit
        Layer2(std::stringstream* ss = nullptr) : _stream(ss) {}

        std::stringstream* _stream;
    };


    static
    void Server1Service1Layer1()
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
        // Log layer events
        //
        std::stringstream stream;

        Layer1 layer1(&stream);
        Layer2 layer2(&stream);
        //
        // Create stack of 2 layers.
        //
        bond::comm::LayerStack<Data, Layer1, Layer2> layers(layer1, layer2);

        //
        // Start transport.
        //
        bond::comm::LayerTransport<Transport, bond::comm::CompactWireProtocol>  transport(layers);

        bond::comm::Server server = transport.Bind(serverAddress, boost::ref(firstService));

        auto client = transport.Connect(serverAddress);
        First::Proxy proxy(client);

        LOOP("Server1Service1Layer1")
        {
            Params params;
            Result result;

            params.op = SyncCallback;

            WaitForResponse<Result> syncMessage;
            proxy.Method1(params, syncMessage);

            syncMessage.Deserialize(result);


            UT_AssertAreEqual(stream.str(), "1:snd-req,2:snd-req,2:rcv-req,1:rcv-req,1:snd-resp,2:snd-resp,2:rcv-resp,1:rcv-resp.");
            std::stringstream emptyStream;
            stream.swap(emptyStream);
        }
    }


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


    static
    void Server1Service1Session2()
    {
        //
        // use two First_ServiceImpl instances in two separate sessions
        // invoke methods of different instances via two proxies.
        //

        uint32_t instance = 0;
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;
        bond::comm::Server server = transport.Bind(serverAddress,
                                             [&instance](typename Transport::ConnectionContext) mutable
                                             {
                                                const int32_t sessionId = ++instance;
                                                return bond::comm::ServiceTable().Register(boost::make_shared<First_ServiceImpl>(sessionId), bond::comm::FastWireProtocol());
                                             });

        First::Proxy proxy1(transport.Connect(serverAddress));
        LOOP("Server1Service1Session2 -- First Loop")
        {
            {
                Result result;

                WaitForResponse<Result> syncMessage;
                proxy1.GetInstance(syncMessage);

                syncMessage.Deserialize(result);
                UT_AssertAreEqual(instance, result.z);
            }
        }

        First::Proxy proxy2(transport.Connect(serverAddress));
        LOOP("Server1Service1Session2 -- Second Loop")
        {
            {
                Result result;

                WaitForResponse<Result> syncMessage;
                proxy2.GetInstance(syncMessage);

                syncMessage.Deserialize(result);
                UT_AssertAreEqual(instance, result.z);
            }
        }
    }

    static
    void Server1Service1Session1()
    {
        //
        // share single First_ServiceImpl instance between two sessions
        // invoke methods of same instance via two proxies.
        //

        const uint32_t instance = 3;

        //
        // Instantiate service tobe shared.
        boost::shared_ptr<First_ServiceImpl> firstService(new First_ServiceImpl(instance));
        bond::comm::SocketAddress serverAddress("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        bond::comm::Server server = transport.Bind(serverAddress,
                                             [firstService](typename Transport::ConnectionContext)
                                             {
                                                return bond::comm::ServiceTable().Register(firstService,
                                                                                           bond::comm::FastWireProtocol());
                                             });

        First::Proxy proxy1(transport.Connect(serverAddress));
        First::Proxy proxy2(transport.Connect(serverAddress));

        LOOP("Server1Service1Session1")
        {
            {
                Result result;

                WaitForResponse<Result> syncMessage;
                proxy1.GetInstance(syncMessage);

                syncMessage.Deserialize(result);
                UT_AssertAreEqual(instance, result.z);
            }

            {
                Result result;

                WaitForResponse<Result> syncMessage;
                proxy2.GetInstance(syncMessage);

                syncMessage.Deserialize(result);
                UT_AssertAreEqual(instance, result.z);
            }
        }
    }

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

        boost::shared_ptr<Second> secondService(new Second_ServiceImpl(&proxy1));

        bond::comm::Server server2 = transport.Bind(serverAddress2, secondService);
        Second::Proxy proxy2(transport.Connect(serverAddress2));

        LOOP("Server2Service2Forward1")
        {
            Params params;
            Result result;

            params.op = ForwardCallback;

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

        boost::shared_ptr<Second> secondService(new Second_ServiceImpl(&proxy1));

        bond::comm::Server server2 = transport.Bind(serverAddress2, secondService);
        Second::Proxy proxy2(transport.Connect(serverAddress2, bond::comm::CompactWireProtocol()));

        LOOP("Server2Service2Forward2")
        {
            Params params;
            Result result;

            params.op = ForwardCallback;

            WaitForResponse<Result> syncMessage;
            proxy2.Method2(params, syncMessage);

            syncMessage.Deserialize(result);
            UT_AssertIsTrue(result.z == 1);
        }
    }

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

        bond::comm::SocketAddress address("127.0.0.1", 9000);


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

        bond::comm::SocketAddress address("127.0.0.1", 9000);


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

        bond::comm::SocketAddress address("127.0.0.1", 9000);


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
        bond::comm::SocketAddress address("127.0.0.1", 9000);
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
    void Server1Service1Void1()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with void response.
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
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with void request.
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
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with void request and void response.
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

        LOOP("Server1Service1Void3")
        {
            WaitForResponse<void> syncMessage;
            proxy.MethodVoid(syncMessage);

            syncMessage.Wait();
        }
    }


    static
    void Server1Service1GenericType1()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic response.
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
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic request.
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
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        // invoke method with generic request and void response.
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

        Generic<unittest::rpc::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService1")
        {
            unittest::rpc::Data data;
            data.m_x = 2;

            WaitForResponse<unittest::rpc::Result> syncMessage;
            proxy.MethodResultGen(boost::cref(data), syncMessage);

            unittest::rpc::Result result;
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

        Generic<unittest::rpc::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService2")
        {
            unittest::rpc::Params params;
            params.x = 2;

            WaitForResponse<unittest::rpc::Data> syncMessage;
            proxy.MethodGenParams(params, syncMessage);

            unittest::rpc::Data result;
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

        Generic<unittest::rpc::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService3")
        {
            unittest::rpc::Data data;
            data.m_x = 2;

            WaitForResponse<unittest::rpc::Data> syncMessage;
            proxy.MethodGen(boost::cref(data), syncMessage);

            unittest::rpc::Data result;
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

        Generic<unittest::rpc::Data>::Proxy proxy(client);

        LOOP("Server1Service1GenericService4")
        {
            bond::Box<unittest::rpc::Data> params;
            params.value.m_x = 2;

            WaitForResponse<bond::Box<unittest::rpc::Data>> syncMessage;
            proxy.MethodBox(params, syncMessage);

            bond::Box<unittest::rpc::Data> result;
            syncMessage.Deserialize(result);

            UT_AssertIsTrue(result.value.m_x == params.value.m_x);
        }
    }

    static
    void Server1Service1VoidException1()
    {
        //
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        //
        // invoke method that throws exception
        // with void response.
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

        LOOP("Server1Service1Void1")
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
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        //
        // invoke method that throws exception
        // with void request.
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

        LOOP("Server1Service1Void2")
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
        // publish First_ServiceImpl at port 9000
        // connect one First::Proxy instance to it.
        //
        // invoke method that throws exception
        // with void request and void response.
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

        LOOP("Server1Service1Void3")
        {
            WaitForResponse<void> syncMessage;
            proxy.MethodVoidException(syncMessage);

            UT_AssertThrows((syncMessage.Wait()), bond::Exception);
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
                params.op = DelayCallback;

                proxy.Method1(params, syncMessage);
                firstService.WaitForMethod1Start();

                //
                // Client will be destroyed here.
                //
            }

            Result result;

            UT_AssertThrows((syncMessage.Deserialize(result)), bond::Exception);

            firstService.WaitForMethod1Complete(); // Wait for delayed messages processed.
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

        suite.AddTestCase(Server1Service1Send1LocalhostAny, "Server1Service1Send1LocalhostAny");

        //suite.AddTestCase(Server1Service1Send1Reconnect, "Server1Service1Send1Reconnect");

        suite.AddTestCase(Server2Service1Send1, "Server2Service1Send1");

        suite.AddTestCase(Server2Service2Send1, "Server2Service2Send1");

        suite.AddTestCase(Server2Service2Send2, "Server2Service2Send2");

        suite.AddTestCase(Server1Service1Event0, "Server1Service1Event0");

        suite.AddTestCase(Server1Service1Event1, "Server1Service1Event1");

        suite.AddTestCase(Server2Service2Forward1, "Server2Service2Forward1");

        suite.AddTestCase(Server2Service2Forward2, "Server2Service2Forward2");

        suite.AddTestCase(Server1Service1Async1, "Server1Service1Async1");

        suite.AddTestCase(Server1Service1UserError1, "Server1Service1UserError1");

        suite.AddTestCase(Server1Service1Exception1, "Server1Service1Exception1");

        suite.AddTestCase(Server1Service1Exception2, "Server1Service1Exception2");

        suite.AddTestCase(Server1Service1Exception3, "Server1Service1Exception3");

        //suite.AddTestCase(Server1Service1DoubleCallback1, "Server1Service1DoubleCallback1");

        suite.AddTestCase(Server1Service1Void1, "Server1Service1Void1");

        suite.AddTestCase(Server1Service1Void2, "Server1Service1Void2");

        suite.AddTestCase(Server1Service1Void3, "Server1Service1Void3");

        suite.AddTestCase(Server1Service1GenericType1, "Server1Service1GenericType1");

        suite.AddTestCase(Server1Service1GenericType2, "Server1Service1GenericType2");

        suite.AddTestCase(Server1Service1GenericType3, "Server1Service1GenericType3");

        suite.AddTestCase(Server1Service1GenericService1, "Server1Service1GenericService1");

        suite.AddTestCase(Server1Service1GenericService2, "Server1Service1GenericService2");

        suite.AddTestCase(Server1Service1GenericService3, "Server1Service1GenericService3");

        suite.AddTestCase(Server1Service1GenericService4, "Server1Service1GenericService4");

        suite.AddTestCase(Server1Service1VoidException1, "Server1Service1VoidException1");

        suite.AddTestCase(Server1Service1VoidException2, "Server1Service1VoidException2");

        suite.AddTestCase(Server1Service1VoidException3, "Server1Service1VoidException3");

        suite.AddTestCase(Server1Service1Timeout1, "Server1Service1Timeout1");

        suite.AddTestCase(Server1Service1Timeout2, "Server1Service1Timeout2");

        suite.AddTestCase(Server1Service1Protocol1, "Server1Service1Protocol1");

        suite.AddTestCase(Server1Service1Protocol2, "Server1Service1Protocol2");

        suite.AddTestCase(Server1Service1Protocol3, "Server1Service1Protocol3");

        suite.AddTestCase(Server1Service1DropException1, "Server1Service1DropException1");

        suite.AddTestCase(Server1Service1Receive1k, "Server1Service1Receive1k");

        suite.AddTestCase(Server1Service1Receive10k, "Server1Service1Receive10k");

        suite.AddTestCase(Server1Service1Receive1M, "Server1Service1Receive1M");

        suite.AddTestCase(Server1Service1Receive10M, "Server1Service1Receive10M");

        suite.AddTestCase(Server1Service1Send1k, "Server1Service1Send1k");

        suite.AddTestCase(Server1Service1Send10k, "Server1Service1Send10k");

        suite.AddTestCase(Server1Service1Send10000k, "Server1Service1Send10000k");

        suite.AddTestCase(Server1Service1Send100B, "Server1Service1Send100B");

        suite.AddTestCase(Server1Service1Session1, "Server1Service1Session1");

        suite.AddTestCase(Server1Service1Session2, "Server1Service1Session2");

        suite.AddTestCase(Server1Service1Layer0, "Server1Service1Layer0");

        suite.AddTestCase(Server1Service1Layer1, "Server1Service1Layer1");
    }
};

namespace TestCore
{
    static
    void WaitForResponseForgotten()
    {
        //
        // Validate common WaitForResponse usage patterns.
        //
        {
            //
            // Safe to destroy unused waiters.
            //
            WaitForResponse<Params> w1;

            WaitForResponse<Params> w2;

            auto callback = w2.Callback();
        }

        {
            //
            // Safe to call forgotten waiters.
            //

            std::function<void (const bond::comm::message<Params>&)> callback;

            {
                WaitForResponse<Params> w;
                callback = w.Callback();;
            }

            Params params;
            callback(boost::cref(params));
        }
    }

    static
    void WaitForResponseMainUsage()
    {
        //
        // Main usage patterns.
        //
        {
            //
            // Same thread.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                w.Callback()(bond::comm::bonded_cast<Params>(in));

                w.Deserialize(out);

                UT_AssertIsTrue(out.x == i);
            }
        }

        {
            //
            // Different thread.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                boost::scoped_thread<boost::join_if_joinable> thread(boost::bind(w.Callback(), bond::comm::bonded_cast<Params>(in)));

                w.Deserialize(out);

                UT_AssertIsTrue(out.x == i);
            }
        }

        {
            //
            // Same thread, with large timeout.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                w.Callback()(bond::comm::bonded_cast<Params>(in));

                w.Deserialize(out, 1000000);

                UT_AssertIsTrue(out.x == i);
            }
        }

        {
            //
            // Different thread, with large timeout.
            //
            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                boost::scoped_thread<boost::join_if_joinable> thread(boost::bind(w.Callback(), bond::comm::bonded_cast<Params>(in)));

                w.Deserialize(out, 10000000);

                UT_AssertIsTrue(out.x == i);
            }
        }
    }

    static
    void WaitForResponseTimeout()
    {
        //
        // Usage patterns with timeout triggered.
        //
        {
            //
            // Same thread.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                auto callback = w.Callback();

                Params out;

                try
                {
                    w.Deserialize(out, 1);

                    UT_AssertIsTrue(false);
                }
                catch (const bond::Exception&)
                {
                }
            }
        }

        {
            //
            // Different thread.
            //
            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                auto callback = w.Callback();

                try
                {
                    Params out;
                    w.Deserialize(out, 1);

                    UT_AssertIsTrue(false);
                }
                catch (const bond::Exception&)
                {
                }

                Params in;
                in.x = i;

                boost::scoped_thread<boost::join_if_joinable> thread(boost::bind(callback, bond::comm::bonded_cast<Params>(in)));
            }
        }
    }


    static
    void ScheduleTimeoutForgotten()
    {
        bond::comm::thread_pool threads;

        auto callback =
            bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                               [](const bond::comm::message<void>&)
                                               {
                                               },
                                               10000);
    }


    static
    void ScheduleTimeoutMainCallback()
    {
        bond::comm::thread_pool threads;
        {
            bool flag = false;
            auto callback =
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&flag](const bond::comm::message<void>& m)
                                                   {
                                                       flag = !m.is_error();
                                                   },
                                                   10000);

            callback(bond::comm::message<void>());
            UT_AssertIsTrue(flag);
        }
    }

    static
    void ScheduleTimeoutElapsed()
    {
        bond::comm::thread_pool threads;
        {
            bool flag = false;
            auto callback =
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&flag](const bond::comm::message<void>& m)
                                                   {
                                                       flag = m.is_error();
                                                   },
                                                   1);
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            callback(bond::comm::message<void>());
            UT_AssertIsTrue(flag);
        }

        {
            bool flag = false;
            auto callback =
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&flag](const bond::comm::message<void>& m)
                                                   {
                                                       flag = m.is_error();
                                                   },
                                                   1);
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            UT_AssertIsTrue(flag);
        }

        {
            //
            // Test if callback may be released within timer thread.
            //
            bool flag = false;
            std::function<void (const bond::comm::message<void>&)>* callback = nullptr;
            callback = new std::function<void (const bond::comm::message<void>&)>(
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&callback, &flag](const bond::comm::message<void>& m)
                                                   {
                                                        delete callback;
                                                        flag = m.is_error();
                                                   },
                                                   10));

            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            UT_AssertIsTrue(flag);
        }
    }


    static
    void Initialize()
    {
        UnitTestSuite suite("Generic RPC utils");

        suite.AddTestCase(WaitForResponseForgotten, "WaitForResponseForgotten");
        suite.AddTestCase(WaitForResponseMainUsage, "WaitForResponseMainUsage");
        suite.AddTestCase(WaitForResponseTimeout, "WaitForResponseTimeout");

        suite.AddTestCase(ScheduleTimeoutForgotten, "ScheduleTimeoutForgotten");
        suite.AddTestCase(ScheduleTimeoutMainCallback, "ScheduleTimeoutMainCallback");
        suite.AddTestCase(ScheduleTimeoutElapsed, "ScheduleTimeoutElapsed");
    }
}


namespace TestUtils
{
    //
    // Implement test transport core to be used for serialization/layers tests.
    //
    struct TestTransport
        : public bond::comm::Transport<std::string, bond::comm::FastWireProtocol>
    {
        // Implement transport core requirements.


        //
        // PacketTransport implementation details.
        //
        boost::shared_ptr<bond::comm::IService> ConnectTo(const std::string&) override
        {
            return m_connection;
        }

        //
        // Server implementation details.
        //
        class Server
        {
        public:
            Server()
            {}
        };


        boost::shared_ptr<void> BindTo(
            const std::string&,
            const std::function<boost::shared_ptr<bond::comm::IService> (const std::string&, const std::string&)>& sessionFactory) override
        {
            m_connection = sessionFactory("", "session");

            return boost::make_shared<Server>();
        }


        TestTransport()
        {}


        TestTransport(int)
        {}

        boost::shared_ptr<bond::comm::IService> m_connection;
    };


    struct Test_ServiceImpl
        : public Test
    {
        void TestMethod1(const bond::comm::payload<Params>&,
                         const std::function<void (const bond::comm::message<Result>&)>& callback) override
        {
            Result r;
            callback(boost::cref(r));
        }

        void TestMethod2(const bond::comm::payload<void>&,
                         const std::function<void (const bond::comm::message<void>&)>& callback) override
        {
            callback(bond::comm::message<void>());
        }

    public:

        Test_ServiceImpl()
        {}
    };


    struct TestConnection : bond::comm::IService
    {
        void Invoke(bond::comm::Request& request, const bond::comm::ResponseCallback& callback) override
        {
            m_connection->Invoke(request,
                                 [&](bond::comm::Response& response)
                                 {
                                    callback(response);
                                 });
        }


        void Notify(bond::comm::Event& event) override
        {
            m_connection->Notify(event);
        }


        TestConnection(const boost::shared_ptr<bond::comm::IService>& connection)
            : m_connection(connection)
        {}

        boost::shared_ptr<bond::comm::IService> m_connection;
    };

    static
    void TestMessageAgainstVariables()
    {
        Test_ServiceImpl service;

        //
        // Test that const reference, reference and value types are compile safe
        // as input parameters to the ctor of message<T&>
        //

        TestTransport transport;

        const std::string address = "127.0.0.1:9000";
        bond::comm::Server server = transport.Bind(address, boost::ref(service));

        boost::shared_ptr<TestConnection> connection = boost::make_shared<TestConnection>(transport.m_connection);
        transport.m_connection = connection;

        Test::Proxy proxy(transport.Connect(""));

        const Params& params = Params();
        proxy.TestMethod1(params,
                          [](const bond::comm::message<Result>&)
                          {});

        proxy.TestMethod1(Params(),
                          [](const bond::comm::message<Result>&)
                          {});

        Params p;
        proxy.TestMethod1(p,
                          [](const bond::comm::message<Result>&)
                          {});
    }


    static
    void TestExpicitSessionSupport()
    {
        Test_ServiceImpl service;

        TestTransport transport;

        std::string instance;

        const std::string address = "127.0.0.1:9000";
        bond::comm::Server server = transport.Bind(address,
                                             [&instance, &service](const std::string& session) mutable -> bond::comm::ServiceTable
                                             {
                                                    instance = session;
                                                    return bond::comm::ServiceTable().Register(boost::ref(service),
                                                                                               bond::comm::FastWireProtocol());
                                             });



        Test::Proxy proxy(transport.Connect(""));

        proxy.TestMethod2([](const bond::comm::message<void>&){});

        UT_AssertIsTrue(instance == "session");
    }


    struct TestLayer
    {
        void OnSend(bond::comm::MessageType, const std::string&, const std::string&, Dummy&)
        {}

        void OnReceive(bond::comm::MessageType, const std::string&, const std::string&, Dummy&)
        {}
    };

    static
    void TestLayerTransportCtors()
    {
        {
            //
            // Test default ctor
            //
            Test_ServiceImpl service;

            bond::comm::LayerTransport<TestTransport, bond::comm::CompactWireProtocol> transport;

            //const std::string address = ;
            bond::comm::Server server = transport.Bind("127.0.0.1:9000", boost::ref(service));

            Test::Proxy proxy(transport.Connect(""));

            proxy.TestMethod2([](const bond::comm::message<void>&) {});
        }

        {
            //
            // Test ctor against base transport args
            //
            Test_ServiceImpl service;

            int a = 0;
            bond::comm::LayerTransport<TestTransport, bond::comm::CompactWireProtocol> transport(a);

            //const std::string address = ;
            bond::comm::Server server = transport.Bind("127.0.0.1:9000", boost::ref(service));

            Test::Proxy proxy(transport.Connect(""));

            proxy.TestMethod2([](const bond::comm::message<void>&) {});
        }

        {
            //
            // Test ctor against layer stack
            //
            Test_ServiceImpl service;

            bond::comm::LayerStack<
                Dummy,
                TestLayer
            > layers;

            bond::comm::LayerTransport<TestTransport, bond::comm::CompactWireProtocol> transport(layers);

            //const std::string address = ;
            bond::comm::Server server = transport.Bind("127.0.0.1:9000", boost::ref(service));

            Test::Proxy proxy(transport.Connect(""));

            proxy.TestMethod2([](const bond::comm::message<void>&) {});
        }

        {
            //
            // Test ctor against stack of layers and base transport args
            //
            Test_ServiceImpl service;

            bond::comm::LayerStack<
                Dummy,
                TestLayer
            > layers;

            int a = 0;
            bond::comm::LayerTransport<TestTransport, bond::comm::CompactWireProtocol> transport(layers, a);

            //const std::string address = ;
            bond::comm::Server server = transport.Bind("127.0.0.1:9000", boost::ref(service));

            Test::Proxy proxy(transport.Connect(""));

            proxy.TestMethod2([](const bond::comm::message<void>&) {});
        }
    }

    static
    void Initialize()
    {
        UnitTestSuite suite("Core RPC classes");

        suite.AddTestCase(TestMessageAgainstVariables, "MessageAgainstVariables");

        suite.AddTestCase(TestExpicitSessionSupport, "ExpicitSessionSupport");

        suite.AddTestCase(TestLayerTransportCtors, "TestLayerTransportCtors");
    }
}; // namespace CoreTests

struct TestScheduler
{
    template<template <typename> class TransportTemplate>
    void operator()(TransportTemplateWrap<TransportTemplate>)
    {
        GenericTransportTests<
            TransportTemplate<bond::comm::FastWireProtocol> >::Initialize();
    }
};

bool init_unit_test()
{
    TestUtils::Initialize();
    TestCore::Initialize();

    // This MPL construct doesn't work on VC12, so we explicitly enumerate the
    // transports instead.
    //  boost::mpl::for_each<TransportList>(TestScheduler());
    GenericTransportTests<test::EpoxyTransport<bond::comm::FastWireProtocol>>::Initialize();
    GenericTransportTests<test::NullTransport<bond::comm::FastWireProtocol>>::Initialize();
    GenericTransportTests<test::ExampleTransport<bond::comm::FastWireProtocol>>::Initialize();
    return true;
}
