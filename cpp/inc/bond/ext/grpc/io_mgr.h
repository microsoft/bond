// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

namespace bond { namespace ext { namespace gRPC {

namespace detail {

    /// @brief Interface that all Bond gRPC++ completion queue tag types
    /// implement.
    struct io_mgr_tag
    {
        virtual ~io_mgr_tag() { }

        /// @brief Called when this instance is dequeued from a completion
        /// queue.
        ///
        /// @param ok whether or not the initial operation succeeded
        virtual void invoke(bool ok) = 0;
    };

} // namespace detail

} } } // namespace bond::ext::gRPC
