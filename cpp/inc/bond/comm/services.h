
#pragma once

#include "message.h"
#include "wire_protocol.h"
#include "detail/future.h"

#include <boost/assert.hpp>
#include <boost/shared_ptr.hpp>

#include <string>
#include <map>

namespace bond { namespace comm
{

typedef
std::function<void (Response&)> ResponseCallback;

//
// Interface abstracting service at different layers of the RPC stack
//
struct IService
{
    virtual
    void Notify(Event& event) = 0;


    virtual
    void Invoke(Request& request,
                const ResponseCallback& callback) = 0;


    virtual
    ~IService()
    {}
};


//
// Stub that forwards requests received from transport layer to user defined Service
//
template <typename Service, typename WireProtocol>
class ServiceStub
    : public IService,
      private boost::noncopyable
{
public:
    ServiceStub(const boost::reference_wrapper<Service>& service,
               const WireProtocol& wire_protocol,
               const std::string& service_name = Service::Schema::metadata.qualified_name)
        : _service(service)
        , _service_name(service_name)
        , _wire_protocol(wire_protocol)
    {
        Register::Methods(*this);
    }


    ServiceStub(const boost::shared_ptr<Service>& service,
               const WireProtocol& wire_protocol,
               const std::string& service_name = Service::Schema::metadata.qualified_name)
        : _service(*service)
        , _service_name(service_name)
        , _wire_protocol(wire_protocol)
        , _serviceInstance(service)
    {
        Register::Methods(*this);
    }


    ServiceStub(const typename Service::Proxy& proxy,
               const WireProtocol& wire_protocol,
               const std::string& service_name = Service::Schema::metadata.qualified_name)
        : _service(_proxyInstance)
        , _service_name(service_name)
        , _wire_protocol(wire_protocol)
        , _proxyInstance(proxy)
    {
        Register::Methods(*this);
    }


    void Notify(Event& event) override
    {
        const std::string& method_name = event.method_name;

        auto it = _notifyTable.find(method_name);
        if (it == _notifyTable.end())
        {
            BOND_THROW(MethodNotFoundException,
                       "Method " << method_name << "@" << _service_name << " is not registered for notify!");
        }

        const NotifyCallback& method = it->second;
        method(event);
    }


    void Invoke(Request& request,
                const ResponseCallback& callback) override
    {
        const std::string& method_name = request.method_name;

        auto it = _invokeTable.find(method_name);
        if (it == _invokeTable.end())
        {
            BOND_THROW(MethodNotFoundException,
                       "Method " << method_name << "@" << _service_name << " is not registered for invoke!");
        }

        const InvokeCallback& method = it->second;
        method(request, callback);
    }

private:
    typedef
    std::function<void (Event&)> NotifyCallback;


    typedef
    std::function<void (Request&, const ResponseCallback&)> InvokeCallback;


    template <typename InPayload>
    struct notify_adaptor
    {
        typedef
        void (Service::*Method)(const payload<InPayload>&);

        void operator()(Event& event)
        {
            (_stub._service.*_method)(_stub._wire_protocol.template Unpack<typename payload<InPayload>::value_type>(event.payload));
        }

        notify_adaptor(const ServiceStub& stub, Method method)
            : _stub(stub)
            , _method(method)
        {}

        const ServiceStub& _stub;
        Method _method;
    };


    template <typename InPayload, typename OutPayload>
    struct invoke_adaptor
    {
        typedef
        void (Service::*Method)(const payload<InPayload>&,
                                const std::function<void (const message<OutPayload>&)>&);

        void operator()(Request& request,
                        const ResponseCallback& callback)
        {
            ProtocolType requestProtocol;
            auto wire_protocol = _stub._wire_protocol;
            auto data = wire_protocol.template Unpack<typename payload<InPayload>::value_type>(request.payload, requestProtocol);

            (_stub._service.*_method)(
                data,
                [wire_protocol, callback, requestProtocol](const message<OutPayload>& msg)
                {
                    Response response;
                    response.is_error = msg.is_error();
                    if (msg.is_error())
                    {
                        response.error = msg.err();
                    }
                    else
                    {
                        response.payload = wire_protocol.Pack(msg.value(), requestProtocol);
                    }
                    callback(response);
                });
        }

        invoke_adaptor(const ServiceStub& stub, Method method)
            : _stub(stub)
            , _method(method)
        {}

        const ServiceStub& _stub;
        Method _method;
    };


    class Register
    {
    public:
        static void Methods(ServiceStub& serviceStub)
        {
            boost::mpl::for_each<typename Service::Schema::methods>(Register(serviceStub));
        }

        template <typename MethodTemplate>
        void operator()(MethodTemplate)
        {
            this->RegisterMethod(MethodTemplate::metadata.name, MethodTemplate::method);
        }

    private:
        Register(ServiceStub& serviceStub)
            : _serviceStub(serviceStub)
        {}

        template <typename InPayload, typename Service_>
        void RegisterMethod(
            const std::string& method_name,
            void (Service_::*method)(const payload<InPayload>&))
        {
            _serviceStub._notifyTable.emplace(
                method_name,
                notify_adaptor<InPayload>(_serviceStub, method));
        }

        template <typename InPayload, typename OutPayload, typename Service_>
        void RegisterMethod(
            const std::string& method_name,
            void (Service_::*method)(const payload<InPayload>&,
                                     const std::function<void (const message<OutPayload>&)>&))
        {
            _serviceStub._invokeTable.emplace(
                method_name,
                invoke_adaptor<InPayload, OutPayload>(_serviceStub, method));
        }

        ServiceStub& _serviceStub;
    };


    friend class ServiceTable;

    Service& _service;
    std::string _service_name;
    std::map<std::string, NotifyCallback> _notifyTable;
    std::map<std::string, InvokeCallback> _invokeTable;
    WireProtocol _wire_protocol;
    typename Service::Proxy _proxyInstance;
    boost::shared_ptr<Service> _serviceInstance;
};


//
// Proxy used to send methods calls over a transport connection
//
template <typename WireProtocol>
class ServiceProxy
{
public:
    ServiceProxy(const boost::shared_ptr<IService>& service, const WireProtocol& writer)
        : _service(service)
        , _wire_protocol(writer)
    {}

    //
    // Pass strong typed request message to untyped layer.
    //
    template <typename InPayload, typename OutPayload>
    void Send(const std::string& service_name,
        const std::string& method_name,
        const payload<InPayload>& input,
        const std::function<void(const message<OutPayload>&)>& callback) const
    {
        auto wire_protocol = _wire_protocol;
        Request request;
        request.payload = wire_protocol.Pack(input.value());
        request.service_name = service_name;
        request.method_name = method_name;

        _service->Invoke(request,
            [callback, wire_protocol](Response& response)
            {
                if (response.is_error)
                {
                    callback(error(response.error));
                }
                else
                {
                    callback(wire_protocol.template Unpack<typename message<OutPayload>::value_type>(response.payload));
                }
            });
    }

    //
    // Pass strong typed event message to untyped layer.
    //
    template <typename InPayload>
    void Send(const std::string& service_name,
        const std::string& method_name,
        const payload<InPayload>& input) const
    {
        Event event;
        event.payload = _wire_protocol.Pack(input.value());
        event.service_name = service_name;
        event.method_name = method_name;

        _service->Notify(event);
    }

private:

    boost::shared_ptr<IService> _service;

    WireProtocol _wire_protocol;
};

} } // namespace bond.comm
