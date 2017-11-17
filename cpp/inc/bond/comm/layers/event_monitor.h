// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "latency.h"


namespace bond { namespace comm
{

struct EventData
{
    //
    // Address of remote endpoint.
    //
    SocketAddress m_address;

    //
    // Request method signature in format method@service.
    //
    std::string m_signature;

    //
    // RpcMessage size in bytes.
    //
    uint32_t m_payloadSize;

    //
    // Network latency in seconds.
    //
    float m_networkLatency;

    //
    // Request to response latency in seconds.
    //
    float m_latency;

    //
    // Payload is exception.
    //
    bool m_exception;
};

struct IEventMonitor
{
    //
    // This method is invoked for every RPC request.
    //
    virtual
    void OnRequest(const EventData& event) = 0;

    //
    // This method is invoked for every RPC response.
    //
    virtual
    void OnResponse(const EventData& event) = 0;

    virtual
    ~IEventMonitor()
    {}
};

struct ClientEventLayer
{
    template <typename Address>
    void Request(const LayerContext<Address>& context)
    {
        if (nullptr != m_instance)
        {
            m_event.m_payloadSize = 0;
            BOOST_FOREACH (const blob& b, context.Payload())
            {
                m_event.m_payloadSize += b.size();
            }


            m_event.m_address = context.Address();
            m_event.m_signature = context.Headers().m_methodSignature.m_methodName +
                                    "@" + context.Headers().m_methodSignature.m_serviceName;
            m_event.m_exception = false;

            m_startTime = detail::GetCurrentDayMilliseconds();
            m_instance->OnRequest(m_event);
        }
    }


    template <typename Address>
    void Response(const LayerContext<Address>& context)
    {
        if (nullptr != m_instance)
        {
            m_event.m_payloadSize = 0;
            BOOST_FOREACH (const blob& b, context.Payload())
            {
                m_event.m_payloadSize += b.size();
            }

            m_event.m_networkLatency = context.Headers().m_networkLatency.m_diff / 1000.f;
            m_event.m_latency = detail::GetMillisecondsOfLatency(m_startTime,
                                                                 detail::GetCurrentDayMilliseconds()) / 1000.f;
            m_event.m_exception = context.IsException();

            m_instance->OnResponse(m_event);
        }
    }


    explicit
    ClientEventLayer(const boost::shared_ptr<IEventMonitor>& instance)
        : m_instance(instance),
          m_event(),
          m_startTime()
    {}

    boost::shared_ptr<IEventMonitor> m_instance;

    EventData m_event;

    uint32_t m_startTime;
};

struct ServerEventLayer
{
    template <typename Address>
    void Request(const LayerContext<Address>& context)
    {
        if (nullptr != m_instance)
        {
            m_event.m_address = context.Address();
            m_event.m_payloadSize = 0;
            BOOST_FOREACH (const blob& b, context.Payload())
            {
                m_event.m_payloadSize += b.size();
            }

            m_event.m_signature = context.Headers().m_methodSignature.m_methodName +
                                    "@" + context.Headers().m_methodSignature.m_serviceName;
            m_event.m_exception = false;

            m_startTime = detail::GetCurrentDayMilliseconds();
            m_instance->OnRequest(m_event);
        }
    }


    template <typename Address>
    void Response(const FilterContext<Address>& context)
    {
        if (nullptr != m_instance)
        {
            m_event.m_payloadSize = 0;
            BOOST_FOREACH (const blob& b, context.Payload())
            {
                m_event.m_payloadSize += b.size();
            }

            m_event.m_latency = detail::GetMillisecondsOfLatency(m_startTime,
                                                                 detail::GetCurrentDayMilliseconds()) / 1000.f;
            m_event.m_exception = context.IsException();

            m_instance->OnResponse(m_event);
        }
    }


    explicit
    ServerEventLayer(const boost::shared_ptr<IEventMonitor>& instance)
        : m_instance(instance),
          m_event(),
          m_startTime()
    {}

    boost::shared_ptr<IEventMonitor> m_instance;

    EventData m_event;

    uint32_t m_startTime;
};

} } // namespace bond.comm
