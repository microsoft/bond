// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/assert.hpp>

namespace bond
{

namespace detail
{

template <typename T>
struct counter
{
    counter()
    {
        // This assert indicates violation of One Definition Rule for T.
        BOOST_ASSERT(!value++);
    }

    static int value;
};


template <typename T>
int counter<T>::value;

}


template <typename One, typename Variant>
struct one_definition
{
    static detail::counter<One> value;
};


template <typename One, typename Variant>
detail::counter<One> one_definition<One, Variant>::value;


}
