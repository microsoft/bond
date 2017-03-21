#pragma once

#if defined (__APPLE__)
    // Work-around: 'OSMemoryBarrier' has been explicitly marked deprecated
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    #include <boost/asio.hpp>
    #pragma GCC diagnostic pop
#elif defined (_MSC_VER)
    #pragma warning(push)
    #pragma warning(disable: 4242) // C4242: 'identifier' : conversion from 'type1' to 'type2', possible loss of data
    #include <boost/asio.hpp>
    #pragma warning(pop)
#else
    #include <boost/asio.hpp>
#endif

#include <boost/thread/scoped_thread.hpp>

#include <list>
#include <thread>

namespace bond { namespace comm
{
//
// Basic thread pool implementation.
//
class thread_pool
    : private boost::asio::io_service
{
public:

    //
    // Constructs thread pool with specified number of threads
    //
    // @param numThreads: total number of threads to be created
    // @remark:
    //     Zero number of concurrent threads will allow simultaneous
    //     execution of as many threads as many CPU/cores are available.
    //     This is default value.
    explicit
    thread_pool(uint32_t numThreads = 0)
        : m_work(*this)
    {
        //
        // If preferred # of threads is 0, use # of cpu cores.
        //
        if (0 == numThreads)
        {
            numThreads = std::thread::hardware_concurrency();
        }

        //
        // Spin working threads.
        //
        for (uint32_t i = 0; i < numThreads; ++i)
        {
            m_threads.emplace_back(
                [this]()
                {
                    this->run();
                });
        }
    }


    //
    // Schedules callback for execution
    //
    // @param callback: functor object to be scheduled.
    //
    template <typename Callback>
    void schedule(Callback&& callback)
    {
        this->post(std::forward<Callback>(callback));
    }

    //
    // Provide io_service
    //
    boost::asio::io_service& get_io_service()
    {
        return *this;
    }

    //
    // Working threads.
    //
    std::list<boost::scoped_thread<boost::join_if_joinable>> m_threads;

    //
    // Helper to keep io_service spinning.
    //
    boost::asio::io_service::work m_work;
};

} } // namespace bond.comm
