// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/*
 *
 * Portions Copyright 2015, Google Inc.
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

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/thread_pool.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpcpp/grpcpp.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/optional/optional.hpp>

#include <memory>
#include <thread>

namespace bond { namespace ext { namespace gRPC {

    template <typename TThreadPool> class server_builder_core;

    /// @brief Models a gRPC server powered by Bond services.
    ///
    /// Servers are configured and started via
    /// bond::ext:gRPC::server_builder.
    template <typename TThreadPool>
    class server_core final
    {
    public:
        ~server_core()
        {
            Shutdown();
            Wait();
        }

        server_core(const server_core&) = delete;
        server_core& operator=(const server_core&) = delete;

        server_core(server_core&&) = default;
        server_core& operator=(server_core&&) = default;

        /// @brief Shutdown the server, blocking until all rpc processing
        /// finishes.
        ///
        /// Forcefully terminate pending calls after \p deadline expires.
        ///
        /// @param deadline How long to wait until pending rpcs are
        /// forcefully terminated.
        template <class T>
        void Shutdown(const T& deadline)
        {
            _grpcServer->Shutdown(deadline);
        }

        /// Shutdown the server, waiting for all rpc processing to finish.
        void Shutdown()
        {
            _grpcServer->Shutdown();
        }

        /// @brief Block waiting for all work to complete.
        ///
        /// @warning The server must be either shutting down or some other
        /// thread must call \p Shutdown for this function to ever return.
        void Wait()
        {
            _grpcServer->Wait();
        }

        friend class server_builder_core<TThreadPool>;

private:
    server_core(
        std::unique_ptr<grpc::Server> grpcServer,
        std::unique_ptr<grpc::ServerCompletionQueue> cq)
        : _grpcServer(std::move(grpcServer)),
          _ioManager(std::move(cq))
    {
        BOOST_ASSERT(_grpcServer);
    }

        std::unique_ptr<grpc::Server> _grpcServer;
        io_manager _ioManager;
    };

    using server = server_core<bond::ext::gRPC::thread_pool>;

} } } //namespace bond::ext::gRPC
