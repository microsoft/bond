// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bond.h"
#include "detail/cmdargs.h"

namespace bond
{
namespace cmd
{
    // Output usage help to std::cerr
    template <typename Options>
    void ShowUsage(const char* program)
    {
        Options options;
        Apply(detail::Usage(program), options);
    }

    // Read command line arguments
    template <typename Options>
    Options GetArgs(int argc, char** argv, bool partial = false)
    {
        Options options;
        Apply(detail::CmdArg(argc, argv, partial), options);
        return options;
    }
}

}
