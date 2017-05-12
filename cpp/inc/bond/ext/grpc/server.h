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

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpc++/grpc++.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/ext/grpc/io_mgr.h>

#include <boost/assert.hpp>
#include <boost/optional/optional.hpp>
#include <memory>
#include <thread>

namespace bond { namespace ext { namespace gRPC {

    /// @brief Models a gRPC server powered by Bond services.
    ///
    /// Servers are configured and started via
    /// bond::ext:gRPC::server_builder.
    class server final
    {
    public:
        ~server()
        {
            if (_ioThread)
            {
                // if we have an _ioThread, then we've been started, so we
                // should request shutdown
                Shutdown();

                // Wait will get rid of the _ioThread.
                Wait();
            }
        }

        server(const server&) = delete;
        server& operator=(const server&) = delete;

        server(server&&) = default;
        server& operator=(server&&) = default;

        /// @brief Shutdown the server, blocking until all rpc processing
        /// finishes.
        ///
        /// Forcefully terminate pending calls after \p deadline expires.
        ///
        /// @param deadline How long to wait until pending rpcs are forcefully
        /// terminated.
        template <class T>
        void Shutdown(const T& deadline)
        {
            _grpcServer->Shutdown(deadline);
            _cq->Shutdown();
        }

        /// Shutdown the server, waiting for all rpc processing to finish.
        void Shutdown()
        {
            _grpcServer->Shutdown();
            _cq->Shutdown();
        }

        /// @brief Block waiting for all work to complete.
        ///
        /// @warning The server must be either shutting down or some other
        /// thread must call \p Shutdown for this function to ever return.
        void Wait()
        {
            _grpcServer->Wait();

            // server_builder should have called start() before returning this,
            // so we should have an IO thread.

            // TODO: Wait() cannot currently be called by multiple threads at
            // once. The following code is not safe to be called from multiple
            // threads. Switching to an io manager like we plan to should fix
            // this.
            BOOST_ASSERT(_ioThread);
            BOOST_ASSERT(_ioThread.get().joinable());
            _ioThread.get().join();
            _ioThread = boost::none;
        }

        friend class server_builder;

    private:
        server(
            std::unique_ptr<grpc::Server> grpcServer,
            std::unique_ptr<grpc::ServerCompletionQueue> cq)
            : _grpcServer(grpcServer.release()),
            _cq(cq.release()),
            _ioThread()
        {
            BOOST_ASSERT(_grpcServer);
            BOOST_ASSERT(_cq);
        }

        void start()
        {
            // TODO: replace with an io_mgr class
            grpc::CompletionQueue* cq = _cq.get();
            _ioThread.emplace([cq]()
            {
                void* tag;
                bool ok;
                while (cq->Next(&tag, &ok))
                {
                    BOOST_ASSERT(tag);
                    static_cast<detail::io_mgr_tag*>(tag)->invoke(ok);
                }
            });
        }

        std::unique_ptr<grpc::Server> _grpcServer;
        std::unique_ptr<grpc::ServerCompletionQueue> _cq;
        boost::optional<std::thread> _ioThread;
    };

} } } //namespace bond::ext::gRPC
