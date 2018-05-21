// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "exception.h"

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
    /// @brief Basic thread pool implementation.
    class basic_thread_pool
    {
    public:
        /// @brief Constructs and starts a thread pool with number of threads equal
        /// to CPU/cores available.
        ///
        /// @throws InvalidThreadCount when std::thread::hardware_concurrency returns 0.
        basic_thread_pool()
            : basic_thread_pool{ std::thread::hardware_concurrency() }
        {}

        /// @brief Constructs and starts a thread pool with the specified number of
        /// threads.
        ///
        /// @throws InvalidThreadCount when 0 is specified.
        explicit basic_thread_pool(unsigned int numThreads)
        {
            if (numThreads == 0)
            {
                throw InvalidThreadCount{};
            }

            auto& service = *_service;
            for (unsigned int i = 0; i < numThreads; ++i)
            {
                service.threads.emplace_back(
                    [&service]
                    {
                        service.run();
                    });
            }
        }

        /// @brief Schedules a callback for execution.
        ///
        /// @param callback: functor object to be scheduled.
        template <typename Callback>
        void operator()(Callback&& callback)
        {
            _service->post(std::forward<Callback>(callback));
        }

        /// @brief Get the underlying boost::asio::io_service
        boost::asio::io_service& get_io_service()
        {
            return *_service;
        }

    private:
        struct service : boost::asio::io_service
        {
            service()
                : work{ *this }
            {}

            /// Working threads.
            std::vector<boost::scoped_thread<>> threads;
            /// Helper to keep io_service spinning.
            boost::asio::io_service::work work;
        };

        std::shared_ptr<service> _service{ std::make_shared<service>() };
    };

} } } // namespace bond::ext::gRPC
