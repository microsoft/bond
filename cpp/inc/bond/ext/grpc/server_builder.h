// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/*
 *
 * Portions Copyright 2015-2016, Google Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following disclaimer
 * in the documentation and/or other materials provided with the
 * distribution.
 *     * Neither the name of Google Inc. nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#pragma once

#include <bond/core/config.h>

#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/detail/service.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpcpp/server_builder.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>

#include <memory>
#include <set>

namespace bond { namespace ext { namespace gRPC {

    /// @brief A builder class for the creation and startup of \a
    /// bond::ext::gRPC::server instances.
    template <typename TThreadPool>
    class server_builder_core final {
    public:
        server_builder_core()
            : _grpcServerBuilder(),
              _services(),
              _threadPool()
        {
        }

        /// Register a service. This call does not take ownership of the
        /// service. The service must exist for the lifetime of the \p
        /// server instance returned by \p BuildAndStart().
        ///
        /// Matches requests with any :authority
        server_builder_core& RegisterService(detail::service<TThreadPool>* service)
        {
            BOOST_ASSERT(service);
            _grpcServerBuilder.RegisterService(service->grpc_service());
            _services.insert(service);

            return *this;
        }

        /// Register a service. This call does not take ownership of the
        /// service. The service must exist for the lifetime of the \p
        /// server instance returned by BuildAndStart().
        ///
        /// Only matches requests with :authority \p host
        server_builder_core& RegisterService(const grpc::string& host, detail::service<TThreadPool>* service)
        {
            BOOST_ASSERT(service);
            _grpcServerBuilder.RegisterService(host, service->grpc_service());
            _services.insert(service);

            return *this;
        }

        /// Set max receive message size in bytes.
        server_builder_core& SetMaxReceiveMessageSize(int max_receive_message_size)
        {
            _grpcServerBuilder.SetMaxReceiveMessageSize(max_receive_message_size);
            return *this;
        }

        /// Set max send message size in bytes.
        server_builder_core& SetMaxSendMessageSize(int max_send_message_size)
        {
            _grpcServerBuilder.SetMaxSendMessageSize(max_send_message_size);
            return *this;
        }

        /// @brief Set the support status for compression algorithms.
        ///
        /// All algorithms are enabled by default.
        ///
        /// Incoming calls compressed with an unsupported algorithm will
        /// fail with GRPC_STATUS_UNIMPLEMENTED.
        server_builder_core& SetCompressionAlgorithmSupportStatus(
            grpc_compression_algorithm algorithm,
            bool enabled)
        {
            _grpcServerBuilder.SetCompressionAlgorithmSupportStatus(algorithm, enabled);
            return *this;
        }

        /// The default compression level to use for all channel calls in
        /// the absence of a call-specific level.
        server_builder_core& SetDefaultCompressionLevel(grpc_compression_level level)
        {
            _grpcServerBuilder.SetDefaultCompressionLevel(level);
            return *this;
        }

        /// The default compression algorithm to use for all channel calls
        /// in the absence of a call-specific level. Note that it overrides
        /// any compression level set by \p SetDefaultCompressionLevel.
        server_builder_core& SetDefaultCompressionAlgorithm(
            grpc_compression_algorithm algorithm)
        {
            _grpcServerBuilder.SetDefaultCompressionAlgorithm(algorithm);
            return *this;
        }

        /// Set the attached buffer pool for this server.
        server_builder_core& SetResourceQuota(const grpc::ResourceQuota& resource_quota)
        {
            _grpcServerBuilder.SetResourceQuota(resource_quota);
            return *this;
        }

        server_builder_core& SetThreadPool(std::shared_ptr<TThreadPool> thread_pool)
        {
            _threadPool = thread_pool;
            return *this;
        }

        /// Tries to bind this server to the given \p addr.
        ///
        /// It can be invoked multiple times.
        ///
        /// @param addr The address to try to bind to the server (eg,
        /// localhost:1234, 192.168.1.1:31416, [::1]:27182, etc.).
        /// @param creds The credentials associated with the server.
        /// @param[out] selected_port Upon success, updated to contain the
        /// port number. \p nullptr otherwise.
        server_builder_core& AddListeningPort(
            const grpc::string& addr,
            std::shared_ptr<grpc::ServerCredentials> creds,
            int* selected_port = nullptr)
        {
            _grpcServerBuilder.AddListeningPort(addr, creds, selected_port);
            return *this;
        }

        /// Return a running server which is ready for processing calls.
        std::unique_ptr<bond::ext::gRPC::server_core<TThreadPool>> BuildAndStart()
        {
            std::unique_ptr<grpc::ServerCompletionQueue> cq =
                _grpcServerBuilder.AddCompletionQueue();
            std::unique_ptr<grpc::Server> server =
                _grpcServerBuilder.BuildAndStart();

            if (!_threadPool)
            {
                _threadPool = std::make_shared<TThreadPool>();
            }

            // Tickle all the services so they queue a receive for all their
            // methods.
            for (auto& service : _services)
            {
                service->start(cq.get(), _threadPool);
            }

            std::unique_ptr<bond::ext::gRPC::server_core<TThreadPool>> result {
                new bond::ext::gRPC::server_core<TThreadPool> {
                    std::move(server),
                    std::move(cq) } };

            return result;
        }

    private:
        grpc::ServerBuilder _grpcServerBuilder;
        std::set<detail::service<TThreadPool>*> _services;
        std::shared_ptr<TThreadPool> _threadPool;
    };

    using server_builder = server_builder_core<bond::ext::gRPC::thread_pool>;

} } } // namespace bond::ext::gRPC
