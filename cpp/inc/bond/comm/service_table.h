
#pragma once

#include "services.h"

namespace bond { namespace comm
{

//
// Table of all exported RPC service interfaces.
//
class ServiceTable
    : public IService
{    
    template <typename WireProtocol>
    class Registrar
    {
    public:
        template <typename Service>
        Registrar Register(const boost::reference_wrapper<Service>& service,
            const std::string& name = Service::Schema::metadata.qualified_name)
        {
            _table.Register(service, _wireProtocol, name);
            return *this;
        }

        template <typename Service>
        Registrar Register(const boost::shared_ptr<Service>& service,
            const std::string& name = Service::Schema::metadata.qualified_name)
        {
            _table.Register(service, _wireProtocol, name);
            return *this;
        }

    private:
        friend class ServiceTable;

        Registrar(ServiceTable& table, const WireProtocol& wireProtocol)
            : _table(table),
              _wireProtocol(wireProtocol)
        {}

        Registrar(const Registrar& that) = default;

        ServiceTable& _table;
        const WireProtocol& _wireProtocol;
    };


public:
    ServiceTable()
    {}

    ServiceTable(const ServiceTable& that)
        : m_table(that.m_table)
    {}

    ServiceTable(ServiceTable&& that)
        : m_table(std::move(that.m_table))
    {}

    template <typename WireProtocol>
    Registrar<WireProtocol> Using(const WireProtocol& wireProtocol)
    {
        return Registrar<WireProtocol>(*this, wireProtocol);
    }
    
    template <typename Service, typename WireProtocol>
    ServiceTable& Register(const boost::reference_wrapper<Service>& service,
                           const WireProtocol& wireProtocol,
                           const std::string& name = Service::Schema::metadata.qualified_name)
    {
        return Register(boost::make_shared<
            ServiceStub<Service, WireProtocol>>(service, wireProtocol, name));
    }


    template <typename Service, typename WireProtocol>
    ServiceTable& Register(const boost::shared_ptr<Service>& service,
                           const WireProtocol& wireProtocol,
                           const std::string& name = Service::Schema::metadata.qualified_name)
    {
        return Register(boost::make_shared<
            ServiceStub<Service, WireProtocol>>(service, wireProtocol, name));
    }


    void Notify(Event& event) override
    {
        GetService(event.service_name)->Notify(event);
    }


    void Invoke(Request& request,
                const ResponseCallback& callback) override
    {
        GetService(request.service_name)->Invoke(request, callback);
    }

private:
    template <typename Service, typename WireProtocol>
    ServiceTable& Register(boost::shared_ptr<ServiceStub<Service, WireProtocol>>&& stub)
    {
        if (!m_table.emplace(stub->_service_name, stub).second)
        {
            // This is an expected exception to indicate that user is trying to register
            // service of a name that already exists in the table.
            BOND_THROW(InvalidInvocationException,
                "Service " << stub->_service_name << " is already inserted!");
        }

        return *this;
    }

    const boost::shared_ptr<IService>& GetService(const std::string& service_name) const
    {
        //
        // Find service in the map.
        //
        auto it = m_table.find(service_name);
        if (it == m_table.end())
        {
            BOND_THROW(MethodNotFoundException,
                       "Service " << service_name << " is not registered!");
        }

        return it->second;
    }

    std::map<std::string, boost::shared_ptr<IService> > m_table;
};

} } // namespace bond.comm
