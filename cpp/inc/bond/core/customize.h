// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "traits.h"

namespace bond
{


template <typename T> struct
is_protocol_enabled
    : std::false_type {};


}
