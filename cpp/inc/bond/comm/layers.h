// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/comm/detail/logging.h>
#include <bond/comm/services.h>

namespace bond { namespace comm
{
template <typename Data, typename... Layers>
class LayerStack;


template <typename Data,
          typename Layer>
class LayerStack<Data, Layer>
    : public Layer
{
public:

    LayerStack()
    {}


    explicit
    LayerStack(const Layer& layer)
        : Layer(layer)
    {}


    void OnSend(MessageType message_type,
                const std::string& service_name,
                const std::string& method_name,
                Data& data)
    {
        Layer::OnSend(message_type, service_name, method_name, data);
    }


    void OnReceive(MessageType message_type,
                  const std::string& service_name,
                  const std::string& method_name,
                  Data& data)
    {
        Layer::OnReceive(message_type, service_name, method_name, data);
    }
};


template <typename Data,
          typename Layer,
          typename... Layers>
class LayerStack<Data, Layer, Layers...>
    : public LayerStack<Data, Layers...>
    , public Layer
{
public:

    LayerStack()
    {}


    explicit
    LayerStack(const Layer& layer, const Layers&... layers)
        : LayerStack<Data, Layers...>(layers...)
        , Layer(layer)
    {}


    void OnSend(MessageType message_type,
                const std::string& service_name,
                const std::string& method_name,
                Data& data)
    {
        Layer::OnSend(message_type, service_name, method_name, data);
        LayerStack<Data, Layers...>::OnSend(message_type, service_name, method_name, data);
    }


    void OnReceive(MessageType message_type,
                   const std::string& service_name,
                   const std::string& method_name,
                   Data& data)
    {
        LayerStack<Data, Layers...>::OnReceive(message_type, service_name, method_name, data);
        Layer::OnReceive(message_type, service_name, method_name, data);
    }
};


//
// Layer extension for transports.
//
template <typename BaseTransport,
          typename LayerProtocol>
class LayerTransport
    : public BaseTransport
    , public LayerProtocol
{
public:

    typedef typename BaseTransport::WireProtocol WireProtocol;
    typedef typename BaseTransport::ConnectionContext ConnectionContext;
    typedef typename BaseTransport::Address Address;


    template <typename... Args>
    explicit
    LayerTransport(Args&&... args)
        : BaseTransport(std::forward<Args>(args)...)
        , _factory(CreateNullFactory())
    {}


    template <typename Data, typename... Layers, typename... Args>
    explicit
    LayerTransport(LayerStack<Data, Layers...> layerStack,
                   Args&&... args)
        : BaseTransport(std::forward<Args>(args)...)
        , _factory(CreateFactory(std::move(layerStack)))
    {}

protected:

    boost::shared_ptr<IService>
    ConnectTo(const Address& address) override
    {
        return _factory(true, *this, address, BaseTransport::ConnectTo(address));
    }


    boost::shared_ptr<void>
    BindTo(const Address& address,
           const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& tableFactory) override
    {
        const WireProtocol& protocol = *this;
        const auto& factory = _factory;
        return BaseTransport::BindTo(
            address,
            [tableFactory, protocol, factory](const Address& remoteAddress, ConnectionContext context) {
                return factory(false, protocol, remoteAddress, tableFactory(remoteAddress, context));
            });
    }

private:

    static
    std::function<boost::shared_ptr<IService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)>
    CreateNullFactory()
    {
        return [](bool, const WireProtocol&, const Address&, const boost::shared_ptr<IService>& service) {
            return service;
        };
    }
    template <typename Data, typename... Layers>
    static
    std::function<boost::shared_ptr<IService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)>
    CreateFactory(const LayerStack<Data, Layers...>& layerStack)
    {
        return
        [layerStack](bool onConnect, const WireProtocol& protocol, const Address& address, const boost::shared_ptr<IService>& service) {
            return onConnect
                ? boost::shared_ptr<IService>(boost::make_shared<Processor<Outgoing, Data, LayerStack<Data, Layers...>>>(address, service, layerStack, protocol))
                : boost::shared_ptr<IService>(boost::make_shared<Processor<Incoming, Data, LayerStack<Data, Layers...>>>(address, service, layerStack, protocol));
        };
    }

    template <typename Policy, typename LayerData, typename LayerStack>
    struct Processor
        : public IService
        , public LayerStack
        , public Policy
    {
        Processor(const Address& address,
                  const boost::shared_ptr<IService>& service,
                  const LayerStack& layerStack,
                  const WireProtocol& protocol)
            : LayerStack(layerStack)
            , Policy(protocol)
            , _service(service)
            , _address(address)
        {}


        void Invoke(Request& request, const ResponseCallback& callback) override
        {
            LayerStack layerStack(static_cast<const LayerStack&>(*this));

            LayerData data;
            Policy policy = *this;
            policy.OnRequest(layerStack,
                             MessageType::REQUEST,
                             request.service_name,
                             request.method_name,
                             data,
                             request.layers);

            _service->Invoke(
                request,
                [policy, layerStack, callback](Response& response) mutable {
                    LayerData data;
                    policy.OnResponse(layerStack,
                                      data,
                                      response.layers);

                    callback(response);
                });
        }


        void Notify(Event& event) override
        {
            LayerStack layerStack(static_cast<const LayerStack&>(*this));

            LayerData data;
            Policy policy = *this;
            policy.OnRequest(layerStack,
                             MessageType::EVENT,
                             event.service_name,
                             event.method_name,
                             data,
                             event.layers);

            _service->Notify(event);
        }


        boost::shared_ptr<IService> _service;

        Address _address;
    };


    struct Incoming : WireProtocol
    {
        Incoming(const WireProtocol& protocol)
            : WireProtocol(protocol)
        {}


        template <typename LayerData, typename LayerStack>
        void OnRequest(LayerStack& layerStack,
                       MessageType type,
                       const std::string& service_name,
                       const std::string& method_name,
                       LayerData& data,
                       std::vector<blob>& buffers)
        {
            _service_name = service_name;
            _method_name = method_name;

            if (buffers.empty())
            {
                BOND_LOG(LOG_WARNING,
                    "LayerTransport",
                    "Layer stack present but no layer data received.");
            }
            else
            {
                WireProtocol::template Unpack<LayerData>(buffers).Deserialize(data);
            }

            layerStack.OnReceive(type, service_name, method_name, data);
        }


        template <typename LayerData, typename LayerStack>
        void OnResponse(LayerStack& layerStack,
                        LayerData& data,
                        std::vector<blob>& buffers)
        {
            layerStack.OnSend(MessageType::RESPONSE, _service_name, _method_name, data);

            buffers = WireProtocol::Pack(bonded<LayerData>(data));
        }

        std::string _service_name;
        std::string _method_name;
    };


    struct Outgoing : WireProtocol
    {
        Outgoing(const WireProtocol& protocol)
            : WireProtocol(protocol)
        {}


        template <typename LayerData, typename LayerStack>
        void OnRequest(LayerStack& layerStack,
                       MessageType type,
                       const std::string& service_name,
                       const std::string& method_name,
                       LayerData& data,
                       std::vector<blob>& buffers)
        {
            _service_name = service_name;
            _method_name = method_name;

            layerStack.OnSend(type, service_name, method_name, data);

            buffers = WireProtocol::Pack(bonded<LayerData>(data));
        }


        template <typename LayerData, typename LayerStack>
        void OnResponse(LayerStack& layerStack,
                        LayerData& data,
                        std::vector<blob>& buffers)
        {
            if (buffers.empty())
            {
                BOND_LOG(LOG_WARNING,
                    "LayerTransport",
                    "Layer stack present but no layer data received.");
            }
            else
            {
                WireProtocol::template Unpack<LayerData>(buffers).Deserialize(data);
            }

            layerStack.OnReceive(MessageType::RESPONSE, _service_name, _method_name, data);
        }

        std::string _service_name;
        std::string _method_name;
    };


    std::function<boost::shared_ptr<IService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> _factory;
}; // class LayerTransport

} } // namespace bond.comm
