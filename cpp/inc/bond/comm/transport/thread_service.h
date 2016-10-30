#pragma once

#include <thread>

#include <boost/asio.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread/thread.hpp>

#include <bond/comm/detail/logging.h>

namespace bond { namespace comm {

// Manage combined lifetime of boost::asio::ioservice and boost::asio::io_service::work
class ThreadService
    : boost::noncopyable
{
public:
    explicit
    ThreadService(uint32_t numberOfThreads = 0)
        : m_ioService(boost::make_shared<boost::asio::io_service>())
        , m_work(*m_ioService)
    {
        InitializeThreads(numberOfThreads);
    }


    ~ThreadService()
    {
        m_ioService->stop();

        //
        // Wait for working threads to complete.
        //
        for (boost::thread& thread : m_threads)
        {
            if (thread.get_id() == boost::this_thread::get_id())
            {
                thread.detach();
            }
            else
            {
                thread.join();
            }
        }
    }


    const boost::shared_ptr<boost::asio::io_service>& io_service() const
    {
        return m_ioService;
    }

private:

    //
    // Initialize working threads.
    //
    void InitializeThreads(uint32_t numberOfThreads)
    {
        //
        // If preferred # of threads is 0, use # of cpu cores.
        //
        if (0 == numberOfThreads)
        {
            numberOfThreads = std::thread::hardware_concurrency();
        }

        m_threads.reserve(numberOfThreads);

        //
        // Spin working threads.
        //
        for (uint32_t i = 0; i < numberOfThreads; ++i)
        {
            m_threads.emplace_back(
                [this]() {
                    m_ioService->run();
            });
        }

        BOND_LOG(LOG_DEBUG, "IOService",
            numberOfThreads << " network threads started, IOService initialized");
    }

    //
    // Asio io service ref.
    //
    boost::shared_ptr<boost::asio::io_service> m_ioService;

    //
    // Helper to keep io_service spinning.
    //
    boost::asio::io_service::work m_work;

    //
    // Working threads.
    //
    std::vector<boost::thread> m_threads;
};

} } // namespace bond.comm