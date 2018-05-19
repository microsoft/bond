// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include <bond/core/bond_types.h>

namespace bond
{
    namespace reflection
    {
        struct nothing;

    } // namespace reflection
    
namespace ext { namespace gRPC { namespace detail
{
    template <typename T>
    struct payload
    {
        using type = T;
    };

    template <>
    struct payload<void>
    {
        using type = Void;
    };

    template <>
    struct payload<bond::reflection::nothing>
        : payload<void>
    {};

} } } } // namespace bond::ext::gRPC::detail
