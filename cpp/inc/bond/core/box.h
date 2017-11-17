// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bond_types.h>

#include <type_traits>


namespace bond
{

template <typename T>
Box<typename std::remove_reference<T>::type> make_box(T&& arg)
{
    Box<typename std::remove_reference<T>::type> b;
    b.value = std::forward<T>(arg);
    return b;
}

} // namespace bond
