// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond
{


// Trait defining what built-it type is aliased by T
template <typename T> struct
aliased_type
{
    typedef void type;
};

#if 0

// Overloaded function to set variable to an aliased value
template <typename T>
void set_aliased_value(T& var, typename aliased_type<T>::type value);


// Overloaded function to get an aliased value
template <typename T>
typename aliased_type<T>::type get_aliased_value(const T& value);

#endif

}
