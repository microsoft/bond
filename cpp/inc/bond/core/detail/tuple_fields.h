// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond
{
namespace detail
{

template <typename Tuple, uint16_t Id, typename T>
struct tuple_field
{
    typedef Tuple struct_type;
    typedef typename std::remove_reference<T>::type value_type;
    typedef typename remove_maybe<value_type>::type field_type;
    typedef reflection::optional_field_modifier field_modifier;

    static const Metadata metadata;
    BOND_STATIC_CONSTEXPR uint16_t id = Id;

    static BOND_CONSTEXPR const value_type& GetVariable(const struct_type& obj)
    {
        return std::get<id>(obj);
    }

    static BOND_CONSTEXPR value_type& GetVariable(struct_type& obj)
    {
        return std::get<id>(obj);
    }

    static Metadata GetMetadata()
    {
        Metadata m;
        m.name = "item" + std::to_string(id);
        return m;
    }
};

using ignore_t = decltype(std::ignore);

template <typename Tuple, uint16_t t_id, typename T>
const Metadata tuple_field<Tuple, t_id, T>::metadata
    = tuple_field<Tuple, t_id, T>::GetMetadata();


template <typename Tuple, uint16_t id, typename ...Rest> struct
tuple_fields;

template <typename Tuple, uint16_t id, typename T, typename ...Rest> struct
tuple_fields<Tuple, id, T, Rest...>
{
    typedef typename boost::mpl::push_front<
        typename tuple_fields<Tuple, id + 1, Rest...>::type,
        tuple_field<Tuple, id, T>
    >::type type;
};

template <typename Tuple, uint16_t id, typename ...Rest> struct
tuple_fields<Tuple, id, const ignore_t&, Rest...>
    : tuple_fields<Tuple, id + 1, Rest...>
{};

template <typename Tuple, uint16_t id> struct
tuple_fields<Tuple, id>
{
    typedef typename boost::mpl::list<>::type type;
};


template <typename ...T> struct
param_list;

template <typename T, typename ...Rest> struct
param_list<T, Rest...>
{
    typedef typename boost::mpl::push_front<
        typename param_list<Rest...>::type,
        typename std::add_pointer<T>::type
    >::type type;
};

template <typename ...Rest> struct
param_list<const ignore_t&, Rest...>
    : param_list<Rest...>
{};

template <> struct
param_list<>
{
    typedef boost::mpl::list<> type;
};

} // namespace detail

} // namespace bond
