
#define BOND_ENABLE_LOG_HANDLER

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <unit_test_comm_utils_reflection.h>
#include <unit_test_comm_utils_comm.h>

#include <bond/comm/transport.h>
#include <bond/comm/layers.h>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"

using namespace unittest::comm;

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


void init_unit_test_comm_utils()
{
    TestUtils::Initialize();
}
