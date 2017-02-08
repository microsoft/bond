
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <session_reflection.h>
#include <session_comm.h>

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
class SessionTransportTests
{
    typedef T Transport;

    class Session_ServiceImpl
        : public SessionService
    {
        void GetInstance(const bond::comm::payload<void>& /* input */,
                         const std::function<void (const bond::comm::message<Result>&)>& callback)
        override
        {
            Result r;
            r.z = m_instance;
            callback(boost::cref(r));
        }

        uint32_t m_instance;

    public:

        Session_ServiceImpl()
            : m_instance()
        {}

        explicit
        Session_ServiceImpl(uint32_t instance)
            : m_instance(instance)
        {}
    };


    static
    void Server1Service1Session2()
    {
        //
        // use two Session_ServiceImpl instances in two separate sessions
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
                                                return bond::comm::ServiceTable().Register(boost::make_shared<Session_ServiceImpl>(sessionId), bond::comm::FastWireProtocol());
                                             });

        SessionService::Proxy proxy1(transport.Connect(serverAddress));
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

        SessionService::Proxy proxy2(transport.Connect(serverAddress));
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
        // share single Session_ServiceImpl instance between two sessions
        // invoke methods of same instance via two proxies.
        //

        const uint32_t instance = 3;

        //
        // Instantiate service tobe shared.
        boost::shared_ptr<Session_ServiceImpl> firstService(new Session_ServiceImpl(instance));
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

        SessionService::Proxy proxy1(transport.Connect(serverAddress));
        SessionService::Proxy proxy2(transport.Connect(serverAddress));

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

        suite.AddTestCase(Server1Service1Session1, "Server1Service1Session1");

        suite.AddTestCase(Server1Service1Session2, "Server1Service1Session2");
    }
};


void init_session()
{
    InitializeTests<SessionTransportTests, bond::comm::FastWireProtocol>();
}
