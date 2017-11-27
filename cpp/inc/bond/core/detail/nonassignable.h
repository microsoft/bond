// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond
{
namespace detail
{
    class nonassignable
    {
    protected:
        nonassignable() {}
        ~nonassignable() {}
    private:
        nonassignable& operator=(const nonassignable&);
    };
}
}
