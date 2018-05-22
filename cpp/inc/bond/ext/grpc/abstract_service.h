// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond { namespace ext { namespace gRPC
{
    namespace detail
    {
        class service;

    } // namespace detail

    /// @brief Base public class that all Bond gRPC++ services inherit.
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

} } } // namespace bond::ext::gRPC
