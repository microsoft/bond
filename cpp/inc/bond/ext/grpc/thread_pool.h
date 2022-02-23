// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#if defined(_WIN32) || defined(WIN32)

#include "win_thread_pool.h"

namespace bond { namespace ext { namespace [[deprecated("Bond-over-gRPC will be removed in the next major version of Bond. See https://github.com/microsoft/bond/issues/1131")]] grpc
{
    using thread_pool = win_thread_pool;

} } } // namespace bond::ext::grpc

#else // defined(_WIN32) || defined(WIN32)

#include "basic_thread_pool.h"

namespace bond { namespace ext { namespace [[deprecated("Bond-over-gRPC will be removed in the next major version of Bond. See https://github.com/microsoft/bond/issues/1131")]] grpc
{
    using thread_pool = basic_thread_pool;

} } } // namespace bond::ext::grpc

#endif
