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

#include "detail/service.h"
#include "exception.h"
#include "io_manager.h"

#include <bond/ext/grpc/service_collection.h>

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

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/range/combine.hpp>

#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace bond { namespace ext { namespace grpc
{
    /// @brief Models a grpc server powered by Bond services.
    ///
    /// Servers are configured and started via bond::ext:grpc::server::Start.
    class server final
    {
    public:
        /// @brief Builds and returns a running server which is ready to process calls
        /// for the provided services.
        template <typename... Services>
        static server Start(::grpc::ServerBuilder& builder, std::unique_ptr<Services>... services)
        {
            service_collection all;
            all.Add(std::move(services)...);
            return Start(builder, std::move(all));
        }

        /// @brief Builds and returns a running server which is ready to process calls
        /// for the provided services.
        static server Start(::grpc::ServerBuilder& builder, service_collection services)
        {
            auto cq = builder.AddCompletionQueue();

            for (const auto& item : boost::combine(services.services(), services.names()))
            {
                auto& service = item.get<0>();

                if (const auto& host = item.get<1>())
                {
                    builder.RegisterService(host.value(), service->grpc_service());
                }
                else
                {
                    builder.RegisterService(service->grpc_service());
                }

                service->SetCompletionQueue(cq.get());
            }

            if (auto svr = builder.BuildAndStart())
            {
                return server{
                    std::move(svr),
                    std::move(services.services()),
                    std::unique_ptr<io_manager>{ new io_manager{
                        std::thread::hardware_concurrency(), /*delay=*/ false, std::move(cq) } } };
            }

            throw ServerBuildException{};
        }

        static server Start(::grpc::ServerBuilder& builder) = delete;

        server(server&&) = default;
        server& operator=(server&&) = default;

        ~server()
        {
            if (_server)
            {
                Shutdown();
                Wait();
            }
        }

        /// @brief Shutdown the server, blocking until all rpc processing
        /// finishes.
        ///
        /// Forcefully terminate pending calls after \p deadline expires.
        ///
        /// @param deadline How long to wait until pending rpcs are
        /// forcefully terminated.
        template <typename T>
        void Shutdown(const T& deadline)
        {
            _server->Shutdown(deadline);
        }

        /// Shutdown the server, waiting for all rpc processing to finish.
        void Shutdown()
        {
            _server->Shutdown();
        }

        /// @brief Block waiting for all work to complete.
        ///
        /// @warning The server must be either shutting down or some other
        /// thread must call \p Shutdown for this function to ever return.
        void Wait()
        {
            _server->Wait();
        }

    private:
        server(
            std::unique_ptr<::grpc::Server> server,
            std::vector<std::unique_ptr<detail::service>> services,
            std::unique_ptr<io_manager> ioManager)
            : _server{ std::move(server) },
              _services{ std::move(services) },
              _ioManager{ std::move(ioManager) }
        {
            BOOST_ASSERT(_server);
            BOOST_ASSERT(_ioManager);

            start();
        }

        void start()
        {
            for (auto& service : _services)
            {
                service->start();
            }
        }

        std::unique_ptr<::grpc::Server> _server;
        std::vector<std::unique_ptr<detail::service>> _services;
        std::unique_ptr<io_manager> _ioManager;
    };

} } } //namespace bond::ext::grpc
