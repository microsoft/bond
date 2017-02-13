
//
// This is an example of a service, that exports void method with no arguments.
//

// Include auto-generated files.
#include "layer_reflection.h"
#include "layer_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>

// Include GUID prerequisites
#include <boost/uuid/uuid.hpp>            
#include <boost/uuid/uuid_generators.hpp> 
#include <boost/uuid/uuid_io.hpp>         

using namespace examples::layer;

// Implement service
class ServiceImpl : public Service
{
    void Method(const bond::comm::payload<void>&,
                const std::function<void(const bond::comm::message<void>&)>& callback) override
    {
        // Respond right away, with void message
        callback(bond::comm::message<void>());
    }
};

// Layer to assign unique id to each request
struct Tracer
{
    void OnSend(bond::comm::MessageType message_type,
                const std::string& /*service_name*/,
                const std::string& /*method_name*/,
                Trace& trace)
    {
        if (bond::comm::MessageType::REQUEST == message_type
            || bond::comm::MessageType::EVENT == message_type)
        {
            trace_id = boost::uuids::random_generator()();

            // This cannot be a response.
            BOOST_VERIFY(bond::comm::MessageType::RESPONSE != message_type);
        }
        else
        {
            // This must be a response.
            BOOST_VERIFY(bond::comm::MessageType::RESPONSE == message_type);
        }

        trace.id = to_string(trace_id);
    }


    void OnReceive(bond::comm::MessageType message_type,
                   const std::string& /*service_name*/,
                   const std::string& /*method_name*/,
                   Trace& trace)
    {
        if (bond::comm::MessageType::REQUEST == message_type
            || bond::comm::MessageType::EVENT == message_type)
        {
            trace_id = boost::uuids::string_generator()(trace.id);
        }
        else
        {
            // This must be a response.
            BOOST_VERIFY(bond::comm::MessageType::RESPONSE == message_type);

            // Except to receive same call id
            BOOST_VERIFY(boost::uuids::string_generator()(trace.id) == trace_id);
        }
    }

    //
    // Store id
    //
    boost::uuids::uuid trace_id;
};


struct Logger
{
    void OnSend(bond::comm::MessageType message_type,
                const std::string& service_name,
                const std::string& method_name,
                Trace& trace)
    {
        std::clog
            << "Send " << ToString(message_type)
            << " to " << method_name << "@" << service_name
            << ", trace_id: " << trace.id 
            << std::endl;
    }

    void OnReceive(bond::comm::MessageType message_type,
                   const std::string& service_name,
                   const std::string& method_name,
                   Trace& trace)
    {
        std::clog
            << "Receive " << ToString(message_type)
            << " to " << method_name << "@" << service_name
            << ", trace_id: " << trace.id
            << std::endl;
    }
};


int BOND_CALL main()
{
    bond::comm::LayerStack<
        Trace,          // Typed layer data
        Tracer,         // Layer to add tracing
        Logger          // Layer to log tracing
    > layers;

    bond::comm::epoxy::EpoxyTransport transport(layers);

    bond::comm::SocketAddress loopback("127.0.0.1", TEST_PORT_1);
    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    Service::Proxy::Using<std::promise> proxy(transport.Connect(loopback));
        
    // Invoke proxy...
    proxy.Method().get().value();

    // Invoke again...
    proxy.Method().get().value();

    return 0;
}


