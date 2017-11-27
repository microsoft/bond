// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/exception.h>

namespace bond { namespace ext { namespace gRPC {

    /// @brief Exception thrown to indicate that a callback has been invoked
    /// multiple times when only one invocation is expected.
    class MultipleInvocationException : public Exception
    {
    public:
        MultipleInvocationException() : Exception("The callback was invoked more than once.") { }
    };

} } } // namespace bond::ext::gRPC
