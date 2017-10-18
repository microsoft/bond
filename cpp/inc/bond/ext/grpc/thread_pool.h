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

namespace bond { namespace ext { namespace gRPC
{

#if 0
/// @brief The interface that a compliant thread pool must implement.
class thread_pool_concept
{
    /// @brief Schedules a callback for execution.
    ///
    /// @warning The scheduled callback must be executed at some point in
    /// the future. Some components use the thread_pool_concept to schedule
    /// the freeing of resources. If a scheduled callback is dropped, these
    /// resources may not be freed.
    ///
    /// @param callback functor object to be scheduled. Must accept any
    /// callable object.
    template <typename Callback>
    void schedule(Callback&& callback);
};
#endif

/// @brief Basic thread pool implementation.
class thread_pool : private boost::asio::io_service
{
public:
    /// @brief Constant to indicate that the number of threads should be
    /// based on the hardware's available concurrency.
    static constexpr size_t USE_HARDWARE_CONC = 0;

    /// @brief Constructs and starts a thread pool with the specified number of
    /// threads.
    ///
    /// @param numThreads total number of threads to be created. If
    /// \ref USE_HARDWARE_CONC then as many threads as CPU/cores are available
    /// will be created.
    explicit
    thread_pool(size_t numThreads = USE_HARDWARE_CONC)
        : _work(*this)
    {
        if (USE_HARDWARE_CONC == numThreads)
        {
            numThreads = static_cast<size_t>(std::thread::hardware_concurrency());
            if (numThreads == 0)
            {
                // hardware_concurrency can return 0 if it can't figure out
                // the hardware concurrency. Use a small number larger than 1.
                const size_t recourseNumThreads = 2;
                numThreads = recourseNumThreads;
            }
        }

        // Spin working threads.
        for (size_t i = 0; i < numThreads; ++i)
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

} } } // namespace bond::ext::gRPC
