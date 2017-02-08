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
class LayerTransportTests
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

            // Reset the stream
            stream.clear();
            stream.str(std::string());
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

        suite.AddTestCase(Server1Service1Layer0, "Server1Service1Layer0");

        suite.AddTestCase(Server1Service1Layer1, "Server1Service1Layer1");
    }
};


bool init_unit_test()
{
    InitializeTests<LayerTransportTests, bond::comm::FastWireProtocol>();
    return true;
}
