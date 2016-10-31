

#include <bond/comm/transport/epoxy.h>
#include <bond/comm/transport/null.h>
#include "../../../examples/cpp/comm/transport/exampletransport.h"

#include <boost/mpl/list.hpp>


namespace test
{
template <typename WireProtocol>
class ExampleTransport
    : public examples::transport::ExampleTransport<WireProtocol>
{
public:

    ExampleTransport()
        : m_shutdown(false)
    {
        m_thread = boost::thread(boost::bind(&ExampleTransport::RunEvents, this));
    }

    ~ExampleTransport()
    {
        m_shutdown = true;
        m_thread.join();
    }

private:

    void RunEvents()
    {
        while (!m_shutdown)
        {
            if (!this->PumpEvent())
            {
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            }
        }
    }

    boost::thread m_thread;

    boost::atomic_bool m_shutdown;
};

template <typename WireProtocol>
using EpoxyTransport = bond::comm::epoxy::detail::EpoxyTransport<WireProtocol>;

template <typename WireProtocol>
using NullTransport = bond::comm::detail::NullTransport<WireProtocol>;
};

template<template <typename> class TransportTemplate>
struct TransportTemplateWrap
{};

typedef
boost::mpl::list
    < TransportTemplateWrap<test::EpoxyTransport>
    , TransportTemplateWrap<test::NullTransport>
    , TransportTemplateWrap<test::ExampleTransport>
    >::type TransportList;

