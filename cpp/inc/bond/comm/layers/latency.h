
#pragma once

#include <bond/comm/layers.h>
#include <boost/assert.hpp>

namespace bond { namespace comm
{

    namespace detail
    {

    const uint64_t MSECONDS_IN_SECOND = 1000;
    const uint64_t SECONDS_IN_DAY = 3600 * 24;
    const uint64_t MSECONDS_IN_DAY = SECONDS_IN_DAY * MSECONDS_IN_SECOND;

    inline
    uint32_t GetCurrentDayMilliseconds()
    {
        uint64_t performanceFrequency = 0,
                 performanceCounter = 0;

        ::QueryPerformanceFrequency((LARGE_INTEGER*) &performanceFrequency);
        ::QueryPerformanceCounter((LARGE_INTEGER*) &performanceCounter);

        uint64_t lastDayTicks = performanceCounter % (performanceFrequency * SECONDS_IN_DAY);
        uint64_t lastDayMilliseconds = (lastDayTicks * MSECONDS_IN_SECOND) / performanceFrequency;

        // value shouldn't overflow DWORD /2, (24 * 3600 * 1000)ms < 2^32
        BOOST_ASSERT(lastDayMilliseconds < (uint32_t(-1) / 2));

        return static_cast<uint32_t>(lastDayMilliseconds);
    }

    inline
    uint32_t GetMillisecondsOfLatency(DWORD startMs, DWORD endMs)
    {
        uint32_t latency = (2 * MSECONDS_IN_DAY + endMs - startMs) % MSECONDS_IN_DAY;
        return (latency >= MSECONDS_IN_DAY / 2) ? 0 : latency;
    }

    }; // namespace


struct ClientNetworkLatencyLayer
{
    ClientNetworkLatencyLayer()
    {}

    void Request(PacketHeaders& headers)
    {
        headers.m_networkLatency.m_diff = detail::GetCurrentDayMilliseconds();
    }

    void Response(PacketHeaders& headers)
    {
        //
        // Update latency header.
        //
        headers.m_networkLatency.m_diff =
            detail::GetMillisecondsOfLatency(headers.m_networkLatency.m_diff,
                                     detail::GetCurrentDayMilliseconds());
    }
};

struct ServerNetworkLatencyLayer
{
    uint32_t m_diff;

    ServerNetworkLatencyLayer()
        : m_diff(0)
    {}

    void Request(PacketHeaders& headers)
    {
        //
        // Snap latency header.
        //
        m_diff = headers.m_networkLatency.m_diff - detail::GetCurrentDayMilliseconds();
        headers.m_networkLatency.m_diff = 0;
    }

    void Response(PacketHeaders& headers)
    {
        //
        // Update latency header.
        //
        headers.m_networkLatency.m_diff = m_diff + detail::GetCurrentDayMilliseconds();
    }
};

} } // namespace bond.comm
