// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <functional>

namespace bond { namespace ext { namespace gRPC
{

#if 0
/// @brief The interface that a compliant scheduler must implement.
class scheduler
{
    /// @brief Schedules a callback for execution.
    ///
    /// @warning The scheduled callback must be executed at some point in
    /// the future. Some components use the scheduler to schedule
    /// the freeing of resources. If a scheduled callback is dropped, these
    /// resources may not be freed.
    ///
    /// @param callback functor object to be scheduled. Must accept any
    /// callable object.
    template <typename Callback>
    void operator()(Callback&& callback);
};
#endif

using Scheduler = std::function<void(const std::function<void()>& func)>;

} } } // namespace bond::ext::gRPC
