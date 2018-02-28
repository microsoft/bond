// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "traits.h"

#include <stdint.h>

namespace bond
{

//
// container traits - specialize for custom containers
//

template <typename T> struct
is_set_container
    : std::false_type {};


template <typename T> struct
is_map_container
    : std::false_type {};


template <typename T> struct
is_list_container
    : std::false_type {};


template <typename T> struct
require_modify_element
    : std::false_type {};


template <typename T> struct
is_string
    : std::false_type {};


template <typename T> struct
is_wstring
    : std::false_type {};


template <typename T> struct
element_type
{
    typedef typename T::value_type type;
};


//
// enumerators - specialize for custom containers
//

template <typename T>
class enumerator;
#if 0
{
    explicit enumerator(T& list);
    bool more() const;
    typename element_type<T>::type& next();
};
#endif


template <typename T>
class const_enumerator;
#if 0
{
    explicit const_enumerator(const T& container);
    bool more() const;
    const typename element_type<T>::type& next();
};
#endif


//
// container functions - overload for custom containers
//

#if 0
template <typename T>
uint32_t container_size(const T& container);

template <typename T>
void resize_list(T& list, uint32_t size);

template <typename T, typename E, typename F>
void modify_element(T& list, E& element, F deserialize);

template <typename T>
void clear_set(T& set);

template <typename S, typename T>
void set_insert(S& set, const T& item);

template <typename T>
void clear_map(T& map);

template <typename M, typename K, typename T>
T& mapped_at(M& map, const K& key);
#endif

//
// string functions - overload for custom strings
//

#if 0
template<typename C, typename T>
const C* string_data(const T& str);

template<typename C, typename T>
C* string_data(T& str);

template<typename T>
uint32_t string_length(const T& str);

template<typename T>
void resize_string(T& str, uint32_t size);
#endif

} // namespace bond
