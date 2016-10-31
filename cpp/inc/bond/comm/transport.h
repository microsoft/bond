
#pragma once

#include "service_table.h"

namespace bond { namespace comm
{

typedef boost::shared_ptr<void> Server;

template <typename Address_, 
          typename WireProtocol_,
          typename ConnectionContext_ = typename boost::call_traits<Address_>::param_type>
class Transport
{
public:

    typedef Address_ Address;
    typedef WireProtocol_ WireProtocol;
    typedef ConnectionContext_ ConnectionContext;


    ServiceProxy<WireProtocol>
    Connect(const Address& address)
    {
        return Connect(address, _wireProtocol);
    }


    template <typename ExplicitWireProtocol>
    ServiceProxy<ExplicitWireProtocol>
    Connect(const Address& address,
            const ExplicitWireProtocol& wireProtocol)
    {
        return ServiceProxy<ExplicitWireProtocol>(
            ConnectTo(address),
            wireProtocol);
    }


    ::bond::comm::Server
    Bind(const Address& address,
         const std::function<ServiceTable (ConnectionContext)>& tableFactory)
    {
        return BindTo(address,
            [tableFactory](const Address&, ConnectionContext ctx) -> boost::shared_ptr<IService> {
                return boost::make_shared<ServiceTable>(tableFactory(std::move(ctx)));
            });
    }


    ::bond::comm::Server
    Bind(const Address& address,
         const ServiceTable& table)
    {
        return Bind(address,
                    [table](ConnectionContext) { return table; });
    }


    template <typename Service, typename ExplicitWireProtocol>
    ::bond::comm::Server
    Bind(const Address& address,
         const boost::reference_wrapper<Service>& service,
         const ExplicitWireProtocol& wireProtocol)
    {
        return Bind(address, ServiceTable().Register(service, wireProtocol));
    }


    template <typename Service, typename ExplicitWireProtocol>
    ::bond::comm::Server
    Bind(const Address& address,
         const boost::shared_ptr<Service>& service,
         const ExplicitWireProtocol& wireProtocol)
    {
        return Bind(address, ServiceTable().Register(service, wireProtocol));
    }


    template <typename Service>
    ::bond::comm::Server
    Bind(const Address& address,
         const boost::reference_wrapper<Service>& service)
    {
        return Bind(address, service, _wireProtocol);
    }


    template <typename Service>
    ::bond::comm::Server
    Bind(const Address& address,
         const boost::shared_ptr<Service>& service)
    {
        return Bind(address, service, _wireProtocol);
    }

protected:

    Transport() = default;


    explicit
    Transport(const WireProtocol& wireProtocol)
        : _wireProtocol(wireProtocol)
    {}

    virtual
    boost::shared_ptr<IService> ConnectTo(const Address& address) = 0;


    virtual
    boost::shared_ptr<void> BindTo(
        const Address& address,
        const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& sessionFactory) = 0;

private:

    WireProtocol _wireProtocol;
};

} } // namespace bond.comm
