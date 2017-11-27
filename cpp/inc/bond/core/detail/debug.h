// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "omit_default.h"

namespace bond
{

namespace detail
{

template <typename T>
class OptionalDefault : boost::noncopyable
{
public:
    OptionalDefault(const T& var)
        : _var(var),
          _default(true)
    {
        boost::mpl::for_each<typename schema<T>::type::fields>(boost::ref(*this));
    }

    operator bool()
    {
        return _default;
    }

    template <typename Field>
    typename boost::enable_if_c<!is_bond_type<typename Field::field_type>::value
                             && std::is_same<typename Field::field_modifier,
                                        reflection::optional_field_modifier>::value>::type
    operator()(const Field&)
    {
        _default = _default && is_default(Field::GetVariable(_var), Field::metadata);
    }


    template <typename Field>
    typename boost::disable_if_c<!is_bond_type<typename Field::field_type>::value
                              && std::is_same<typename Field::field_modifier,
                                         reflection::optional_field_modifier>::value>::type
    operator()(const Field&)
    {}

private:
    const T& _var;
    bool     _default;
};


} // namespace detail

} // namespace bond
