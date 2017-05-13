
#pragma once

#include "services.h"
#include <boost/weak_ptr.hpp>

namespace bond { namespace comm
{

//
// Base interface for interception points delegated to clients layers
//
// (???) Maybe it will be useful to split all appended methods into separate interfaces
// and implement this interface as a variadic template which extends them.
// It will give transports the ability to implement arbitrary number of interception points
struct ILayerService : public IService
{
	enum EConnectionStatus { ConnClosed, ConnDropped, ConnEstablished };

	virtual void OnConnStateChanged(EConnectionStatus status, bool &reconnect) = 0;
};

//
// Wrapper for holder IService object. Delegates ILayer:: methods to holder. All others ignored.
// Used for transports that does not support extended ILayerService methods
//
struct LayerServiceStub : public ILayerService
{
protected:
	boost::weak_ptr< IService > service;
public:
	LayerServiceStub(const boost::weak_ptr< IService > &_service)
		: service(_service)
	{}
	inline void Notify(Event& event) override
	{
		if (auto serv = service.lock())
			serv->Notify(event);
	}
	inline void Invoke(Request& request, const ResponseCallback& callback) override
	{
		if (auto serv = service.lock())
			serv->Invoke(request, callback);
	}
	inline void OnConnStateChanged(EConnectionStatus, bool &) override {}
};

//
// Base class for transports extended ILayerService support implementation
//
template<typename Address>
class LayerProvider
{
	// factories for creating ILayerService object. By default delegates provided ILayer object
	typedef std::function<boost::shared_ptr<ILayerService>(const Address&, const boost::shared_ptr<IService>&)> layer_serv_func;
	layer_serv_func m_client_factory;
	layer_serv_func m_server_factory;
public:
	LayerProvider() : m_client_factory([](const Address& /*address*/, const boost::shared_ptr<IService>& serv)
			{ return boost::make_shared< LayerServiceStub >(serv); })
		, m_server_factory([](const Address& /*address*/, const boost::shared_ptr<IService>& serv)
			{ return boost::make_shared< LayerServiceStub >(serv); })
	{}
	void SetLayerServiceClientFunc(const layer_serv_func& ls_factory)
	{
		m_client_factory = ls_factory;
	}
	void SetLayerServiceServerFunc(const layer_serv_func& ls_factory)
	{
		m_server_factory = ls_factory;
	}
	//
	// Provides the correct sequence of objects destructure
	//
	struct ClientGarbageCollector : public ILayerService
	{
		boost::shared_ptr<IService> service;
		boost::shared_ptr<ILayerService> layers_serv;
		ClientGarbageCollector(const boost::shared_ptr<ILayerService> &_layers_serv, const boost::shared_ptr<IService> &serv)
			: layers_serv(_layers_serv), service(serv)
		{}
		boost::shared_ptr<ILayerService> GetLayersService() { return layers_serv; }
		inline void Notify(Event& event) override { layers_serv->Notify(event); }
		inline void Invoke(Request& request, const ResponseCallback& callback) override { layers_serv->Invoke(request, callback); }
		inline void OnConnStateChanged(EConnectionStatus status, bool &reconnect) override { layers_serv->OnConnStateChanged(status, reconnect); }
	};
	boost::shared_ptr<ClientGarbageCollector> CreateLayerClientService(const Address& address, const boost::shared_ptr<IService>& serv)
	{
		return boost::make_shared< ClientGarbageCollector >(m_client_factory(address, serv), serv);
	}
	boost::shared_ptr<ILayerService> CreateLayerServerService(const Address& address, const boost::shared_ptr<IService>& serv)
	{
		return boost::make_shared< ClientGarbageCollector >(m_server_factory(address, serv), serv);
	}
};

namespace v2
{

	// base class for a layer intercepting OnSend message
	template< typename LayerData >
	struct OnSendLayerService
	{
		virtual void OnSend(bond::comm::MessageType message_type,
			const std::string&  service_name,
			const std::string&  method_name,
			LayerData& layer_data) = 0;
	};

	// base class for a layer intercepting OnReceive message
	template< typename LayerData >
	struct OnReceiveLayerService
	{
		virtual void OnReceive(bond::comm::MessageType  message_type,
			const std::string&  service_name,
			const std::string&  method_name,
			LayerData& layer_data) = 0;
	};

	// base class for a layer intercepting OnConnStateChanged message
	struct OnConnStateChangedLayerService
	{
		// reconnect flag is meaningful only on ConnDropped state for client
		virtual void OnConnStateChanged(ILayerService::EConnectionStatus status, bool &reconnect) = 0;
	};

} // namespace bond.comm.v2

} } // namespace bond.comm
