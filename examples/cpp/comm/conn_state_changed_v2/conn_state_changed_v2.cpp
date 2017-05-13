
//
// This is an example of a service, that exports void method with no arguments.
//

// Include auto-generated files.
#include "conn_state_changed_v2_reflection.h"
#include "conn_state_changed_v2_comm.h"

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

// layer for OnConnStateChanged interception
struct OnConnChangedTestLayer : public bond::comm::v2::OnConnStateChangedLayerService
{
	const std::string name;
	std::shared_ptr< std::promise<void> > connected;
	OnConnChangedTestLayer(const std::string &_name)
		: name(_name)
	{
		connected = std::make_shared< std::promise<void> >();
	}

	OnConnChangedTestLayer(const OnConnChangedTestLayer& other)
		: name(other.name)
		, connected(other.connected)
	{}

	void OnConnStateChanged(bond::comm::ILayerService::EConnectionStatus status, bool &reconnect) override
	{
		std::cout << "[" << name << "]: OnConnStateChanged: ";
		switch (status)
		{
		case bond::comm::ILayerService::ConnClosed:
			std::cout << "closed";
			break;
		case bond::comm::ILayerService::ConnDropped:
			std::cout << "dropped";
			reconnect = true;
			break;
		case bond::comm::ILayerService::ConnEstablished:
			std::cout << "established";
			connected->set_value();
			break;
		default:
			std::cout << "UNKNOWN";
			break;
		}
		std::cout << std::endl;
	}

	void WaitForConnectionEstablished()
	{
		connected->get_future().wait();
		std::promise<void> d;
		connected->swap(d);
	}

};


int BOND_CALL main()
{
	bond::comm::SocketAddress loopback("127.0.0.1", 25188);

	OnConnChangedTestLayer on_conn_chgd_server_layer("server::on_conn_state_changed");
	bond::comm::v2::LayerStack<
		Trace
		, OnConnChangedTestLayer
	> server_layers(on_conn_chgd_server_layer);
	bond::comm::epoxy::v2::EpoxyTransport serv_transport(server_layers);
	bond::comm::Server server;
	std::thread([&server, &serv_transport, &loopback]()
	{
		std::this_thread::sleep_for(std::chrono::milliseconds(5000));
		server = serv_transport.Bind(loopback, boost::make_shared<ServiceImpl>());
	}).detach();

	OnConnChangedTestLayer on_conn_chgd_client_layer("client::on_conn_state_changed");
	bond::comm::v2::LayerStack<
		Trace
		, OnConnChangedTestLayer
	> client_layers(on_conn_chgd_client_layer);

	bond::comm::epoxy::v2::EpoxyTransport client_transport(client_layers);

	Service::Proxy::Using< std::promise > proxy(client_transport.Connect(loopback));

	on_conn_chgd_client_layer.WaitForConnectionEstablished();

	// Invoke proxy...
	proxy.Method().get().value();

	// Invoke again...
	proxy.Method().get().value();

	return 0;
}
