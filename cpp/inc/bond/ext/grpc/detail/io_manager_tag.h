// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond { namespace ext { namespace grpc { namespace detail {

    /// @brief Interface for completion queue tag types that \ref io_manager
    /// expects.
    ///
    /// Typically, a type inherits from this type, captures at construction
    /// time in its locals the state of some operation, and resumes that
    /// operation in its implementation of \ref invoke.
    struct io_manager_tag
    {
        io_manager_tag() = default;

        io_manager_tag(const io_manager_tag& other) = delete;
        io_manager_tag& operator=(const io_manager_tag& other) = delete;

        virtual ~io_manager_tag() = default;

        /// @brief Called when this instance is dequeued from a completion
        /// queue.
        ///
        /// @param ok whether or not the initial operation succeeded
        virtual void invoke(bool ok) = 0;

        /// @return Returns a %tag value suitable for passing to completion queue routines.
        io_manager_tag* tag()
        {
            return this;
        }
    };

} } } } // namespace bond::ext::grpc::detail
