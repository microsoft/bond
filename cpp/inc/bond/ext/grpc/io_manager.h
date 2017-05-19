// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpc++/grpc++.h>
#include <grpc++/impl/codegen/completion_queue.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/ext/detail/event.h>
#include <bond/ext/grpc/detail/io_manager_tag.h>

#include <boost/assert.hpp>

#include <atomic>
#include <memory>
#include <thread>
#include <vector>

namespace bond { namespace ext { namespace gRPC {

    /// @brief Manages a pool of threads polling for work from the same
    /// %grpc::CompletionQueue
    ///
    /// All of the tags enqueued in this completion queue must inherit from
    /// \ref io_manager_tag. If not, the behavior is undefined.
    class io_manager final
    {
    public:
        /// @param cq the completion queue to poll. Takes ownership.
        ///
        /// @param numThreads the number of threads to start. If 0, then a
        /// number of threads depending on the hardware's available
        /// concurrency will be started.
        explicit io_manager(std::unique_ptr<grpc::CompletionQueue> cq, size_t numThreads = 0)
            : _cq(std::move(cq)),
            _numThreads(compute_real_num_threads(numThreads)),
            _threads(),
            _isShutdownRequested(),
            _isShutdownInProgress(),
            _shutdownCompleted()
        {
            BOOST_ASSERT(_cq);
        }

        /// Waits for the \p io_manager to stop.
        ~io_manager()
        {
            shutdown();
            wait();
        }

        /// Gets the underlying completion queue.
        ///
        /// @note Ownership remains with the completion queue.
        grpc::CompletionQueue* cq()
        {
            return _cq.get();
        }

        /// @brief Starts polling the completion queue.
        ///
        /// @remarks An io_manager can only be started once.
        ///
        /// @remarks An io_manager cannot be restarted after it has been
        /// shutdown.
        void start()
        {
            BOOST_ASSERT(_cq);
            BOOST_ASSERT(_threads.empty());

            _threads.reserve(_numThreads);

            for (size_t i = 0; i < _numThreads; ++i)
            {
                _threads.emplace_back([this]()
                {
                    void* tag;
                    bool ok;
                    while (_cq->Next(&tag, &ok))
                    {
                        BOOST_ASSERT(tag);
                        static_cast<detail::io_manager_tag*>(tag)->invoke(ok);
                    }
                });
            }
        }

        /// @brief Requests that the io_manager shutdown.
        ///
        /// @remarks If the io_manager is being used for by a
        /// bond::ext::gRPC::server, that server needs to be shutdown first.
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

        /// Waits for remaining work to drain from the io_manager and for the
        /// worker threads to be shutdown.
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
            bool shouldShutdown = !_isShutdownInProgress.test_and_set();
            if (shouldShutdown)
            {
                // borrow the current thread to clean up
                for (auto& thread : _threads)
                {
                    BOOST_ASSERT(thread.joinable());
                    thread.join();
                }

                _threads.clear();

                _cq.reset();
                _shutdownCompleted.set();
            }
            else
            {
                // some other thread is performing clean up, so wait for it
                _shutdownCompleted.wait();
            }
        }

    private:
        static size_t compute_real_num_threads(size_t numThreads)
        {
            if (numThreads == 0)
            {
                numThreads = static_cast<size_t>(std::thread::hardware_concurrency());
            }

            // hardware_concurency can fail. If so, we need a non-zero number of threads.
            const size_t recourseNumThreads = 2;
            return numThreads != 0 ? numThreads : recourseNumThreads;
        }

        std::unique_ptr<grpc::CompletionQueue> _cq;
        size_t _numThreads;
        std::vector<std::thread> _threads;

        std::atomic_flag _isShutdownRequested;
        std::atomic_flag _isShutdownInProgress;
        bond::ext::detail::event _shutdownCompleted;
    };

} } } // namespace bond::ext::gRPC
