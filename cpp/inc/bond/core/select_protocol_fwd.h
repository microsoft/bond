// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bond_fwd.h"

#include <utility>

namespace bond
{

// Use compile-time schema
template <typename T, typename Protocols = BuiltInProtocols, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> SelectProtocolAndApply(Buffer& input, const Transform& transform);


// Use runtime schema
template <typename Protocols = BuiltInProtocols, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> SelectProtocolAndApply(
    const RuntimeSchema& schema,
    Buffer& input,
    const Transform& transform);

} // namespace bond
