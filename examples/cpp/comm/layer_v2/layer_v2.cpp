
//
// This is an example of a service, that exports void method with no arguments.
//

// Include auto-generated files.
#include "layer_v2_reflection.h"
#include "layer_v2_comm.h"

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

// layer for OnSend interception
template< typename LayerData >
struct OnSendTestLayer : public bond::comm::v2::OnSendLayerService< LayerData >
{
	const std::string name;
	OnSendTestLayer(const std::string &_name)
		: name(_name)
	{}

	OnSendTestLayer(const OnSendTestLayer& other)
		: name(other.name)
	{}

	void OnSend(bond::comm::MessageType  /*message_type*/,
		const std::string&  /*service_name*/,
		const std::string&  /*method_name*/,
		LayerData& /*layer_data*/) override
	{
		std::cout << "[" << name << "]: OnSend" << std::endl;
	}

};

// layer for OnReceive interception
template< typename LayerData >
struct OnReceiveTestLayer : public bond::comm::v2::OnReceiveLayerService< LayerData >
{
	const std::string name;
	OnReceiveTestLayer(const std::string &_name)
		: name(_name)
	{}

	OnReceiveTestLayer(const OnReceiveTestLayer& other)
		: name(other.name)
	{}

	void OnReceive(bond::comm::MessageType  /*message_type*/,
		const std::string&  /*service_name*/,
		const std::string&  /*method_name*/,
		LayerData& /*layer_data*/) override
	{
		std::cout << "[" << name << "]: OnReceive" << std::endl;
	}

};

// layer for both OnSend & OnReceive interceptions
template< typename LayerData >
struct OnSendReceiveTestLayer
	: public bond::comm::v2::OnReceiveLayerService< LayerData >
	, public bond::comm::v2::OnSendLayerService< LayerData >
{
	const std::string name;
	OnSendReceiveTestLayer(const std::string &_name)
		: name(_name)
	{}

	OnSendReceiveTestLayer(const OnSendReceiveTestLayer& other)
		: name(other.name)
	{}

	void OnReceive(bond::comm::MessageType  /*message_type*/,
		const std::string&  /*service_name*/,
		const std::string&  /*method_name*/,
		LayerData& /*layer_data*/) override
	{
    std::cout << "[" << name << "]: OnReceive" << std::endl;
  }

	void OnSend(bond::comm::MessageType  /*message_type*/,
		const std::string&  /*service_name*/,
		const std::string&  /*method_name*/,
		LayerData& /*layer_data*/) override
	{
    std::cout << "[" << name << "]: OnSend" << std::endl;
  }

};


int BOND_CALL main()
{

	OnSendTestLayer< Trace > on_send_server_layer("server::on_send");
	OnReceiveTestLayer< Trace > on_receive_server_layer("server::on_receive");
  OnSendReceiveTestLayer< Trace > on_send_receive_server_layer( "server::on_send_receive");
  bond::comm::v2::LayerStack<
    Trace
    , OnSendTestLayer< Trace >
    , OnReceiveTestLayer< Trace >
    , OnSendReceiveTestLayer< Trace >
  > server_layers(on_send_server_layer, on_receive_server_layer, on_send_receive_server_layer);
	bond::comm::epoxy::v2::EpoxyTransport serv_transport(server_layers);
  bond::comm::SocketAddress loopback("127.0.0.1", TEST_PORT_1);
  bond::comm::Server server = serv_transport.Bind(loopback, boost::make_shared<ServiceImpl>());

	OnSendTestLayer< Trace > on_send_client_layer("client::on_send");
	OnReceiveTestLayer< Trace > on_receive_client_layer("client::on_receive");
	OnSendReceiveTestLayer< Trace > on_send_receive_client_layer("client::on_send_receive");
	bond::comm::v2::LayerStack<
		Trace
		, OnSendTestLayer< Trace >
		, OnReceiveTestLayer< Trace >
		, OnSendReceiveTestLayer< Trace >
	> client_layers(on_send_client_layer, on_receive_client_layer, on_send_receive_client_layer);

	bond::comm::epoxy::v2::EpoxyTransport client_transport(client_layers);

	Service::Proxy::Using< std::promise > proxy(client_transport.Connect(loopback));

  // Invoke proxy...
  proxy.Method().get().value();

  // Invoke again...
  proxy.Method().get().value();

  return 0;
}
