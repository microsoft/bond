// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#if !defined(_WIN32) && !defined(WIN32)
#error This header is Windows-specific
#endif

#include <sdkddkver.h>

#if _WIN32_WINNT >= 0x0600   // Thread pool API is available on Windows Vista+

#include <bond/core/config.h>

#include <boost/optional.hpp>

#include <windows.h>        // TODO: Avoid including windows.h in public ones

#include <memory>
#include <system_error>
#include <type_traits>

namespace bond { namespace ext { namespace grpc
{
    namespace detail
    {
        BOND_NORETURN void Win32Exception(const char* message)
        {
            throw std::system_error{
                std::error_code{ static_cast<int>(::GetLastError()), std::system_category() },
                message };
        }

    } // namespace detail

    /// @brief Thread pool implementation based on Windows API.
    class win_thread_pool
    {
    public:
        /// @brief Creates new instance of thread pool with predefined number of min/max threads
        ///
        /// @throws std::system_error on failure.
        win_thread_pool()
            : win_thread_pool{ pool_type::new_instance }
        {}

        /// @brief Creates new instance of thread pool with provided number of min/max threads.
        ///
        /// @throws std::system_error on failure.
        win_thread_pool(uint32_t minThreads, uint32_t maxThreads)
            : win_thread_pool{ pool_type::new_instance, std::make_pair(minThreads, maxThreads) }
        {}

        /// @brief Returns default thread pool associated with current process.
        ///
        /// @throws std::system_error on failure.
        static win_thread_pool& get_default()
        {
            static win_thread_pool s_default{ pool_type::default_instance };
            return s_default;
        }

        /// @brief Schedules a callback for execution.
        ///
        /// @param callback: functor object to be scheduled.
        template <typename Callback>
        void operator()(Callback&& callback)
        {
            _impl->schedule(std::forward<Callback>(callback));
        }

    private:
        enum class pool_type
        {
            new_instance,
            default_instance
        };

        class impl
        {
        public:
            impl(pool_type type, boost::optional<std::pair<uint32_t, uint32_t>> numThreads)
                : _env{ nullptr, ::DestroyThreadpoolEnvironment },
                  _pool{ nullptr, ::CloseThreadpool },
                  _group{ nullptr, ::CloseThreadpoolCleanupGroup }
            {
                ::InitializeThreadpoolEnvironment(&_envInst);
                _env.reset(&_envInst);

                if (type == pool_type::new_instance)
                {
                    _pool.reset(::CreateThreadpool(nullptr));
                    if (!_pool)
                    {
                        detail::Win32Exception("Failed to create thread pool.");
                    }

                    ::SetThreadpoolCallbackPool(_env.get(), _pool.get());
                }

                _group.reset(::CreateThreadpoolCleanupGroup());
                if (!_group)
                {
                    detail::Win32Exception("Failed to create thread pool cleanup group.");
                }

                ::SetThreadpoolCallbackCleanupGroup(_env.get(), _group.get(), nullptr);

                if (numThreads)
                {
                    SetThreadCount(static_cast<DWORD>(numThreads->first), static_cast<DWORD>(numThreads->second));
                }
            }

            ~impl()
            {
                // Wait for all callbacks to return without canceling pending once.
                ::CloseThreadpoolCleanupGroupMembers(_group.get(), FALSE, nullptr);
            }

            impl(const impl& other) = delete;
            impl& operator=(const impl& other) = delete;

            template <typename Callback>
            void schedule(Callback&& callback)
            {
                using Func = typename std::decay<Callback>::type;

                std::unique_ptr<Func> func{ new Func{ std::forward<Callback>(callback) } };

                if (!::TrySubmitThreadpoolCallback(
                        static_cast<::PTP_SIMPLE_CALLBACK>([](::PTP_CALLBACK_INSTANCE, PVOID context)
                        {
                            (*std::unique_ptr<Func>{ static_cast<Func*>(context) })();
                        }),
                        func.get(),
                        _env.get()))
                {
                    detail::Win32Exception("Failed to submit thread pool callback.");
                }

                func.release();
            }

        private:
            void SetThreadCount(DWORD minThreads, DWORD maxThreads)
            {
                if (!::SetThreadpoolThreadMinimum(_pool.get(), static_cast<DWORD>(minThreads)))
                {
                    detail::Win32Exception("Failed to set thread pool minimum number of threads.");
                }

                ::SetThreadpoolThreadMaximum(_pool.get(), static_cast<DWORD>(maxThreads));
            }

            ::TP_CALLBACK_ENVIRON _envInst;
            std::unique_ptr<::TP_CALLBACK_ENVIRON, decltype(&::DestroyThreadpoolEnvironment)> _env;
            std::unique_ptr<::TP_POOL, decltype(&::CloseThreadpool)> _pool;
            std::unique_ptr<::TP_CLEANUP_GROUP, decltype(&::CloseThreadpoolCleanupGroup)> _group;
        };

        explicit win_thread_pool(
            pool_type type,
            boost::optional<std::pair<uint32_t, uint32_t>> numThreads = {})
            : _impl{ std::make_shared<impl>(type, numThreads) }
        {}

        std::shared_ptr<impl> _impl;
    };

} } } // namespace bond::ext::grpc

#else // _WIN32_WINNT < 0x0600

#include "basic_thread_pool.h"

namespace bond { namespace ext { namespace grpc
{
    using win_thread_pool = basic_thread_pool;

} } } // namespace bond::ext::grpc

#endif
