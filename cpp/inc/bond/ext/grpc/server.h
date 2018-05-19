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
#include <string>
#include <thread>
#include <vector>

namespace bond { namespace ext { namespace gRPC
{
    /// @brief Models a gRPC server powered by Bond services.
    ///
    /// Servers are configured and started via bond::ext:gRPC::server::Start.
    class server final
    {
    public:
        /// @brief Builds and returns a running server which is ready to process calls
        /// for the provided services.
        template <typename... Services>
        static server Start(grpc::ServerBuilder& builder, std::unique_ptr<Services>... services)
        {
            return starter{ builder }.Start(std::move(services)...);
        }

        /// @brief Builds and returns a running server which is ready to process calls
        /// for the provided services.
        template <typename Service>
        static server Start(grpc::ServerBuilder& builder, std::vector<std::unique_ptr<Service>> services)
        {
            return starter{ builder }.Start(std::move(services));
        }

        /// @brief Builds and returns a running server which is ready to process calls
        /// for the provided services.
        template <typename... Services>
        static server Start(grpc::ServerBuilder& builder, named_service<Services>... namedServices)
        {
            return starter{ builder }.Start(std::move(namedServices)...);
        }

        /// @brief Builds and returns a running server which is ready to process calls
        /// for the provided services.
        template <typename Service>
        static server Start(grpc::ServerBuilder& builder, std::vector<named_service<Service>> namedServices)
        {
            return starter{ builder }.Start(std::move(namedServices));
        }

        static server Start(grpc::ServerBuilder& builder) = delete;

        server(server&&) = default;
        server & operator=(server&&) = default;

        ~server()
        {
            Shutdown();
            Wait();
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
        class starter;

        server(
            std::unique_ptr<grpc::Server> server,
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

        std::unique_ptr<grpc::Server> _server;
        std::vector<std::unique_ptr<detail::service>> _services;
        std::unique_ptr<io_manager> _ioManager;
    };


    class server::starter
    {
    public:
        explicit starter(grpc::ServerBuilder& builder)
            : _builder{ builder }
        {}

        template <typename... Services>
        server Start(std::unique_ptr<Services>... services)
        {
            return Build(Register(Convert<std::unique_ptr<detail::service>>(std::move(services)...)));
        }

        template <typename Service>
        server Start(std::vector<std::unique_ptr<Service>> services)
        {
            return Build(Register(Convert<std::unique_ptr<detail::service>>(std::move(services))));
        }

        template <typename... Services>
        server Start(named_service<Services>... services)
        {
            return Build(Register(Convert<named_service<detail::service>>(std::move(services)...)));
        }

        template <typename Service>
        server Start(std::vector<named_service<Service>> services)
        {
            return Build(Register(Convert<named_service<detail::service>>(std::move(services))));
        }

    private:
        template <typename ServiceImpl, typename... Services>
        static std::vector<ServiceImpl> Convert(Services... services)
        {
            std::vector<ServiceImpl> converted;
            std::initializer_list<int>{
                (converted.emplace_back(detail::service_cast(std::move(services))), 0)... };
            return converted;
        }

        template <typename ServiceImpl, typename Service>
        static std::vector<ServiceImpl> Convert(std::vector<Service> services)
        {
            std::vector<ServiceImpl> converted;
            converted.reserve(services.size());

            for (auto& s : services)
            {
                converted.push_back(detail::service_cast(std::move(s)));
            }

            return converted;
        }

        template <typename ServiceImpl>
        static std::vector<ServiceImpl> Convert(std::vector<ServiceImpl> services)
        {
            return services;
        }

        std::vector<std::unique_ptr<detail::service>>
        Register(std::vector<std::unique_ptr<detail::service>> services)
        {
            for (auto& service : services)
            {
                _builder.RegisterService(service->grpc_service());
            }

            return services;
        }

        std::vector<std::unique_ptr<detail::service>>
        Register(std::vector<named_service<detail::service>> services)
        {
            std::vector<std::unique_ptr<detail::service>> converted;
            converted.reserve(services.size());

            for (auto& pair : services)
            {
                auto service = std::move(pair.second);
                _builder.RegisterService(pair.first, service->grpc_service());
                converted.push_back(std::move(service));
            }

            return converted;
        }

        server Build(std::vector<std::unique_ptr<detail::service>> services)
        {
            auto cq = _builder.AddCompletionQueue();

            for (auto& service : services)
            {
                service->SetCompletionQueue(cq.get());
            }

            if (auto grpc_server = _builder.BuildAndStart())
            {
                return server{
                    std::move(grpc_server),
                    std::move(services),
                    std::unique_ptr<io_manager>{ new io_manager{
                        std::thread::hardware_concurrency(), /*delay=*/ false, std::move(cq) } } };
            }

            throw ServerBuildException{};
        }

        grpc::ServerBuilder& _builder;
    };

} } } //namespace bond::ext::gRPC
