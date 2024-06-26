// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include <bond/core/exception.h>
#include <stdint.h>

namespace bond
{
namespace detail
{

template<class T>
struct RecursionGuardStaticsHolder
{
    /// @brief Maximum depth allowed for recursion
    static uint32_t maxDepth;

    /// @brief Current recursion depth
    thread_local static uint32_t currentDepth;
};

template<class T>
uint32_t RecursionGuardStaticsHolder<T>::maxDepth = 64;

template<class T>
thread_local uint32_t RecursionGuardStaticsHolder<T>::currentDepth;

/// @brief Tracks recursive depth, incrementing a thread_local on construction and decrementing it on destruction, throwing if depth exceeds max depth
class RecursionGuard : RecursionGuardStaticsHolder<void>
{
public:
    RecursionGuard()
    {
        uint32_t depth = currentDepth;
        if (depth >= maxDepth)
        {
            bond::ExceededMaxRecursionDepthException();
        }

        currentDepth = depth + 1;
    }

    ~RecursionGuard()
    {
        currentDepth--;
    }

    /// @brief Sets the maximum recursion depth permitted
    static void SetMaxDepth(uint32_t value)
    {
        maxDepth = value;
    }
};

} // namespace detail
} // namespace bond
