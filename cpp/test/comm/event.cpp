
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <event_reflection.h>
#include <event_comm.h>

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
class EventTransportTests
{
    typedef T Transport;

    class Event_ServiceImpl
        : public EventService
    {
        void Event(const bond::comm::payload<Params>& event)
        override
        {
            Params p;
            event.value().Deserialize(p);
            m_eventParam = p.x;

            m_eventInvoke.Notify();
        }

    public:

        Event_ServiceImpl()
            : m_eventParam()
        {}

        uint32_t GetEventParam() const
        {
            m_eventInvoke.Wait();
            return m_eventParam;
        }

    private:

        mutable Semaphore m_eventInvoke;

        uint32_t m_eventParam;
    };


    static
    void Server1Service1Event0()
    {
        //
        // Create service.
        Event_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);


        bond::comm::ServiceTable table;
        table.Register(boost::ref(firstService), bond::comm::FastWireProtocol());

        //
        // Start transport.
        //
        Transport transport;

        auto client = transport.Connect(address);

        EventService::Proxy proxy(client);

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
        // publish Event_ServiceImpl at port 9000
        // connect one EventService::Proxy instance to it.
        // invoke method.
        //

        //
        // Create service.
        Event_ServiceImpl firstService;
        bond::comm::SocketAddress address("127.0.0.1", 9000);

        //
        // Start transport.
        //
        Transport transport;

        // Publish Event_Service.
        bond::comm::Server server = transport.Bind(address, boost::ref(firstService));

        auto client = transport.Connect(address);

        EventService::Proxy proxy(client);

        LOOP("Server1Service1Event1")
        {
            Params params;
            params.x = 1;

            proxy.Event(params);

            UT_AssertIsTrue(firstService.GetEventParam() == params.x);
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

        suite.AddTestCase(Server1Service1Event0, "Server1Service1Event0");

        suite.AddTestCase(Server1Service1Event1, "Server1Service1Event1");
    }
};


void init_event()
{
    InitializeTests<EventTransportTests, bond::comm::FastWireProtocol>();
}
