
#pragma once

#include <bond/comm/message.h>
#include <bond/comm/thread_pool.h>

#include <boost/asio/steady_timer.hpp>
#include <boost/atomic/atomic.hpp>
#include <chrono>
#include <functional>
#include <memory>

namespace bond { namespace comm
{
//
// Helper functions to handle application timeouts.
//
namespace detail
{

//
// Implementation based on boost::asio::steady_timer.
//
template<typename T>
class ScheduleTimeoutImpl
    : private boost::noncopyable
{
public:
    ScheduleTimeoutImpl(boost::asio::io_service& io_service,
                        const std::function<void(const message<T>&)>& callbackTimeoutOrResponse,
                        const std::function<void(const message<T>&)>& callbackLateResponse,
                        uint32_t msTimeout)
        : m_msTimeout(msTimeout)
        , m_callbackTimeoutOrResponse(callbackTimeoutOrResponse)
        , m_callbackLateResponse(callbackLateResponse)
        , m_counter(0)
        , m_timer(io_service, std::chrono::milliseconds(msTimeout))
    {
        m_timer.async_wait(boost::bind(&ScheduleTimeoutImpl::OnElapsedTimer, this, _1));
    }

    void Callback(const message<T>& msg)
    {
        //
        // Handle external callback.
        //

        //
        // Swap callback to stack storage.
        //
        std::function<void(const message<T>&)> callback;

        //
        // Check if already dispatched to the master callback.
        //
        if (++m_counter == 1)
        {
            swap(callback, m_callbackTimeoutOrResponse);
        }
        else
        {
            //
            // If master callback was already invoked,
            // pass parameter to optional event sink.
            //
            swap(callback, m_callbackLateResponse);
        }

        if (callback)
        {
            callback(msg);
        }
    }


private:

    void OnElapsedTimer(const boost::system::error_code& ec)
    {
        if (!!ec)
        {
            //
            // Ignore canceled timers.
            //
            return;
        }

        //
        // If master callback was already invoked, ignore timer event.
        //
        if (++m_counter > 1)
        {
            return;
        }

        //
        // If master callback is missing, ignore timer event.
        //
        if (!m_callbackTimeoutOrResponse)
        {
            return;
        }

        //
        // Swap callback to stack storage.
        //
        std::function<void(const message<T>&)> callback;
        swap(callback, m_callbackTimeoutOrResponse);

        //
        // Notify callback.
        //
        callback(error(ErrorCode::TIMEOUT_ERROR,
                       (bond::detail::string_stream() << "Time elapsed: " << m_msTimeout << "ms").str()));
    }

    const uint32_t m_msTimeout;

    std::function<void(const message<T>&)> m_callbackTimeoutOrResponse;

    std::function<void(const message<T>&)> m_callbackLateResponse;

    boost::atomic_int32_t m_counter;

    boost::asio::steady_timer m_timer;
};

} // namespace detail


//
// Helper function to trigger exception against callback if wait time elapsed, and handles late responses:
//  @param io_service: io service instance to be used by timer scheduling;
//  @param callbackTimeoutOrResponse: master callback to recieve in-time response, or timeout exception if time elapsed;
//  @param msTimeout: time to elapse before timeout exception is triggered;
//  @param callbackLateResponse: callback to collect all late responses after master callback was invoked;
//
//  @return: functor to be used as callback of responses.
//
template<typename T>
std::function<void (const message<T>&)>
schedule_timeout(boost::asio::io_service& io_service,
                 const std::function<void (const message<T>&)>& callbackTimeoutOrResponse,
                 uint32_t msTimeout,
                 const std::function<void (const message<T>&)>& callbackLateResponse = nullptr)
{
    return boost::bind(&detail::ScheduleTimeoutImpl<T>::Callback,
                       std::make_shared<detail::ScheduleTimeoutImpl<T> >(io_service,
                                                                         callbackTimeoutOrResponse,
                                                                         callbackLateResponse,
                                                                         msTimeout),
                       _1);
}

} } // namespace bond.comm
