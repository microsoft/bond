// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/comm/comm_types.h>
#include <exception>

namespace bond { namespace comm
{

struct Request
{
    std::string service_name;

    std::string method_name;

    std::vector<blob> layers;

    std::vector<blob> payload;
};

typedef Request Event;

struct Response
{
    Error error;

    std::vector<blob> layers;

    std::vector<blob> payload;

    bool is_error;
};

} } // namespace bond.comm
