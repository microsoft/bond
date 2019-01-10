// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/io_manager_tag.h"
#include "exception.h"
#include <bond/core/detail/once.h>

#ifdef _MSC_VER
    #pragma warning (push)
    // warning C4100: unreferenced formal parameter
    //
    // warning C4127: conditional expression is constant
    //
    // warning C4702: unreachable code
    //
    // warning C4800: 'int': forcing value to bool 'true' or 'false' (performance warning)
    #pragma warning (disable: 4100 4127 4702 4800)
#endif

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/completion_queue.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/thread/scoped_thread.hpp>

#include <atomic>
#include <memory>
#include <thread>
#include <vector>

namespace bond { namespace ext { namespace grpc
{
    namespace detail
    {
        class client;

    } // namespace detail

    /// @brief Manages a pool of threads polling for work from the same
    /// %::grpc::CompletionQueue
    ///
    /// All of the tags enqueued in this completion queue must inherit from
    /// \ref detail::io_manager_tag. If not, the behavior is undefined.
    class io_manager final
    {
    public:
        /// @brief Creates and starts and io_manager with number of threads equal
        /// to CPU/cores available.
        ///
        /// @throws InvalidThreadCount when std::thread::hardware_concurrency returns 0.
        io_manager()
            : io_manager{ std::thread::hardware_concurrency() }
        {}

        /// @brief Creates and starts and io_manager with specified number of threads.
        ///
        /// @param numThreads the number of threads to start.
        ///
        /// @param delay a flag to indicate if start should be delayed.
        ///
        /// @param cq the completion queue to poll.
        ///
        /// @throws InvalidThreadCount when std::thread::hardware_concurrency returns 0.
        explicit io_manager(unsigned int numThreads, bool delay = false, std::unique_ptr<::grpc::CompletionQueue> cq = {})
            : _cq{ cq ? std::move(cq) : std::unique_ptr<::grpc::CompletionQueue>{ new ::grpc::CompletionQueue{} } },
              _threads{ numThreads }
        {
            if (_threads.empty())
            {
                throw InvalidThreadCount{};
            }

            if (!delay)
            {
                start();
            }
        }

        /// Waits for the \p io_manager to stop.
        ~io_manager()
        {
            shutdown();
            wait();
        }

        /// Gets the underlying completion queue.
        ///
        /// @note Ownership of the completion queue remains with the
        /// io_manager.
        ::grpc::CompletionQueue* cq()
        {
            return _cq.get();
        }

        /// @brief Starts polling the completion queue.
        ///
        /// @remarks Can be called multiple times, but not safe to be called
        /// concurrently.
        ///
        /// @remarks An io_manager cannot be restarted after it has been
        /// shutdown.
        void start()
        {
            BOOST_ASSERT(_cq);
            BOOST_ASSERT(!_threads.empty());

            for (auto& t : _threads)
            {
                if (!t.joinable())
                {
                    t = boost::scoped_thread<>{ [this]{ run(); } };
                }
            }
        }

        /// @brief Requests that the io_manager shutdown.
        ///
        /// @remarks If the io_manager is being used for by a
        /// bond::ext::grpc::server, that server needs to be shutdown first.
        ///
        /// @remarks Can be called from multiple threads concurrently.
        void shutdown()
        {
            bool shouldRequest = !_isShutdownRequested.test_and_set();
            if (shouldRequest)
            {
                _cq->Shutdown();
            }
        }

        /// Waits for remaining work to drain from the io_manager and for
        /// the worker threads to be shutdown.
        ///
        /// @remarks Can be called from multiple threads concurrently.
        ///
        /// @warning Cannot be called from an io_manager worker thread. In
        /// other words, never call this function from inside of
        /// io_manager_tag::invoke.
        ///
        /// @warning Either \ref shutdown must have been called already, or
        /// some other thread must call \p shutdown for this function to
        /// return.
        void wait()
        {
            bond::detail::call_once(_waitFlag, [this]{ _threads.clear(); });
        }

    private:
        friend class detail::client;

        const std::shared_ptr<::grpc::CompletionQueue>& shared_cq() const
        {
            return _cq;
        }

        void run()
        {
            void* tag;
            bool ok;
            while (_cq->Next(&tag, &ok))
            {
                BOOST_ASSERT(tag);
                static_cast<detail::io_manager_tag*>(tag)->invoke(ok);
            }
        }

        std::shared_ptr<::grpc::CompletionQueue> _cq;
        std::vector<boost::scoped_thread<>> _threads;

        std::atomic_flag _isShutdownRequested = ATOMIC_FLAG_INIT;
        bond::detail::once_flag _waitFlag{};
    };

} } } // namespace bond::ext::grpc
