// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond { namespace ext { namespace [[deprecated("Bond-over-gRPC will be removed in the next major version of Bond. See https://github.com/microsoft/bond/issues/1131")]] grpc
{
    namespace detail
    {
        class service;

    } // namespace detail

    /// @brief Base public class that all Bond grpc++ services inherit.
    class abstract_service
    {
    public:
        virtual ~abstract_service() = default;

        abstract_service(const abstract_service& other) = delete;
        abstract_service& operator=(const abstract_service& other) = delete;

    private:
        friend class detail::service;

        abstract_service() = default;
    };

} } } // namespace bond::ext::grpc
