// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

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

#include <thread>
#include <vector>

namespace bond { namespace ext
{
/// @brief Basic thread pool implementation.
class thread_pool
    : private boost::asio::io_service
{
public:

    /// @brief Constructs and starts a thread pool with the specified number of
    /// threads.
    ///
    /// @param numThreads total number of threads to be created. If zero
    /// then as many threads as many CPU/cores are available will be
    /// created.
    explicit
    thread_pool(uint32_t numThreads = 0)
        : _work(*this)
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
            _threads.emplace_back(
                [this]()
                {
                    this->run();
                });
        }
    }


    /// @brief Schedules a callback for execution.
    ///
    /// @param callback: functor object to be scheduled.
    template <typename Callback>
    void schedule(Callback&& callback)
    {
        this->post(std::forward<Callback>(callback));
    }

    /// @brief Get the underlying boost::asio::io_service
    boost::asio::io_service& get_io_service()
    {
        return *this;
    }

private:

    /// Working threads.
    std::vector<boost::scoped_thread<boost::join_if_joinable>> _threads;

    /// Helper to keep io_service spinning.
    boost::asio::io_service::work _work;
};

} } // namespace bond.ext
