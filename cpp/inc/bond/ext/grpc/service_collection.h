// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/service.h"

#include <boost/assert.hpp>
#include <boost/optional/optional.hpp>

#include <memory>
#include <string>
#include <vector>

namespace bond { namespace ext { namespace [[deprecated("Bond-over-gRPC will be removed in the next major version of Bond. See https://github.com/microsoft/bond/issues/1131")]] grpc
{
    /// @brief A collection of services that is used to construct a server.
    class service_collection final
    {
    public:
        template <typename... Service>
        void Add(std::unique_ptr<Service>... services)
        {
            (void)std::initializer_list<int>{ (Add(boost::none, std::move(services)), 0)... };
        }

        template <typename Service>
        void Add(const std::string& host, std::unique_ptr<Service> service)
        {
            Add(boost::make_optional(host), std::move(service));
        }

        void Add() = delete;

    private:
        friend class server;

        std::vector<boost::optional<std::string>>& names()
        {
            return _names;
        }

        std::vector<std::unique_ptr<detail::service>>& services()
        {
            return _services;
        }

        template <typename Service>
        void Add(const boost::optional<std::string>& host, std::unique_ptr<Service> svc)
        {
            BOOST_STATIC_ASSERT(std::is_base_of<abstract_service, Service>::value);
            BOOST_ASSERT(svc);

            _services.emplace_back(static_cast<detail::service*>(svc.get()));
            svc.release();

            try
            {
                _names.push_back(host);
            }
            catch (...)
            {
                _services.pop_back();
                throw;
            }
        }

        std::vector<boost::optional<std::string>> _names;
        std::vector<std::unique_ptr<detail::service>> _services;
    };

} } } //namespace bond::ext::grpc
