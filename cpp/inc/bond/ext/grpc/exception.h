// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/exception.h>

namespace bond { namespace ext { namespace gRPC {

    /// @brief %Exception thrown to indicate that a callback has been
    /// invoked multiple times when only one invocation is expected.
    class MultipleInvocationException : public Exception
    {
    public:
        MultipleInvocationException()
            : Exception{ "The callback was invoked more than once." }
        {}
    };

    /// @brief %Exception thrown when std::thread::hardware_concurrency
    /// returns 0.
    class InvalidThreadCount : public Exception
    {
    public:
        InvalidThreadCount()
            : Exception{ "Invalid number of threads." }
        {}
    };

} } } // namespace bond::ext::gRPC
