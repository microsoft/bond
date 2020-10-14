// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "container_interface.h"
#include "traits.h"

#include <boost/utility/enable_if.hpp>

#include <list>
#include <map>
#include <set>
#include <stdint.h>
#include <string>
#include <vector>

// Bond container interface on top of STL container classes

namespace bond
{

template <typename Key>
[[noreturn]] void ElementNotFoundException(const Key& key);

// is_string<std::basic_string<char, T, A> >
template<typename T, typename A> struct
is_string<std::basic_string<char, T, A> >
    : std::true_type {};


// is_wstring<std::basic_string<wchar_t, T, A> >
template<typename T, typename A> struct
is_wstring<std::basic_string<wchar_t, T, A> >
    : std::true_type {};


// is_string_type
template <typename T> struct
is_string_type
    : std::integral_constant<bool,
        is_string<typename std::remove_const<T>::type>::value
        || is_wstring<typename std::remove_const<T>::type>::value> {};


// is_list_container<std::list<T, A> >
template <typename T, typename A> struct
is_list_container<std::list<T, A> >
    : std::true_type {};


// is_list_container<std::vector<T, A> >
template <typename T, typename A> struct
is_list_container<std::vector<T, A> >
    : std::true_type {};


// require_modify_element<std::vector<bool, A> >
template <typename A> struct
require_modify_element<std::vector<bool, A> >
    : std::true_type {};


// is_set_container<std::set<T, C, A> >
template <typename T, typename C, typename A> struct
is_set_container<std::set<T, C, A> >
    : std::true_type {};


// is_map_container<std::map<K, T, C, A> >
template <typename K, typename T, typename C, typename A> struct
is_map_container<std::map<K, T, C, A> >
    : std::true_type {};


// specialize element_type for map becuase map::value_type is pair<const K, T>
template <typename K, typename T, typename C, typename A> struct
element_type<std::map<K, T, C, A> >
{
    typedef typename std::pair<K, T> type;
};


// string_data
template<typename C, typename T, typename A>
inline
const C* string_data(const std::basic_string<C, T, A>& str)
{
    return str.data();
}


template<typename C, typename T, typename A>
inline
C* string_data(std::basic_string<C, T, A>& str)
{
    // C++11 disallows COW string implementation (see [string.require] 21.4.1)
    // however it was permitted in C++03. In order to support COW we can't
    // return data() here.
    static C c;
    return str.size() ? &*str.begin() : &c;
}


// string_length
template<typename C, typename T, typename A>
inline
uint32_t string_length(const std::basic_string<C, T, A>& str)
{
    return static_cast<uint32_t>(str.length());
}


// resize_string
template<typename C, typename T, typename A>
inline
void resize_string(std::basic_string<C, T, A>& str, uint32_t size)
{
    str.resize(size);
}


// container_size
template <typename T>
inline
uint32_t container_size(const T& list)
{
    return static_cast<uint32_t>(list.size());
}


// use_container_allocator_for_elements
template <typename T, typename Enable = void> struct
use_container_allocator_for_elements
    : std::false_type {};


template <typename T> struct
use_container_allocator_for_elements<T, typename boost::enable_if_c<
    std::uses_allocator<typename element_type<T>::type, typename T::allocator_type>::value>::type>
    : std::true_type {};


// make_element
template <typename T>
inline
typename boost::disable_if_c<use_container_allocator_for_elements<T>::value, typename element_type<T>::type>::type
make_element(T& /*container*/)
{
    return typename element_type<T>::type();
}


template <typename T>
inline
typename boost::enable_if<use_container_allocator_for_elements<T>, typename element_type<T>::type>::type
make_element(T& container)
{
    return typename element_type<T>::type(container.get_allocator());
}


// resize_list
template <typename T>
inline
typename boost::disable_if<use_container_allocator_for_elements<T> >::type
resize_list(T& list, uint32_t size)
{
    list.resize(size, make_element(list));
}


template <typename T>
inline
typename boost::enable_if<use_container_allocator_for_elements<T> >::type
resize_list(T& list, uint32_t size)
{
    list.clear();
    list.resize(size, make_element(list));
}


// modify_element
template <typename A, typename F>
inline
void modify_element(std::vector<bool, A>&,
                    typename std::vector<bool, A>::reference element,
                    F deserialize)
{
    bool value;

    deserialize(value);
    element = value;
}


// clear_set
template <typename T, typename C, typename A>
inline
void clear_set(std::set<T, C, A>& set)
{
    set.clear();
}


// set_insert
template <typename T, typename C, typename A>
inline
void set_insert(std::set<T, C, A>& set, const T& item)
{
    set.insert(item);
}


// clear_map
template <typename K, typename T, typename C, typename A>
inline
void clear_map(std::map<K, T, C, A>& map)
{
    map.clear();
}


// use_map_allocator_for_keys
template <typename T, typename Enable = void> struct
use_map_allocator_for_keys
    : std::false_type {};


template <typename T> struct
use_map_allocator_for_keys<T, typename boost::enable_if<
    std::uses_allocator<typename element_type<T>::type::first_type, typename T::allocator_type> >::type>
    : std::true_type {};


// make_key
template <typename T>
inline
typename boost::disable_if<use_map_allocator_for_keys<T>, typename element_type<T>::type::first_type>::type
make_key(T& /*map*/)
{
    return typename element_type<T>::type::first_type();
}


template <typename T>
inline
typename boost::enable_if<use_map_allocator_for_keys<T>, typename element_type<T>::type::first_type>::type
make_key(T& map)
{
    return typename element_type<T>::type::first_type(map.get_allocator());
}


// use_map_allocator_for_values
template <typename T, typename Enable = void> struct
use_map_allocator_for_values
    : std::false_type {};


template <typename T> struct
use_map_allocator_for_values<T, typename boost::enable_if<
    std::uses_allocator<typename element_type<T>::type::second_type, typename T::allocator_type> >::type>
    : std::true_type {};


// make_value
template <typename T>
inline
typename boost::disable_if_c<use_map_allocator_for_values<T>::value,
    typename element_type<T>::type::second_type>::type
make_value(T& /*map*/)
{
    return typename element_type<T>::type::second_type();
}


template <typename T>
inline
typename boost::enable_if<use_map_allocator_for_values<T>, typename element_type<T>::type::second_type>::type
make_value(T& map)
{
    return typename element_type<T>::type::second_type(map.get_allocator());
}


// mapped_at
template <typename K, typename T, typename C, typename A>
inline
T& mapped_at(std::map<K, T, C, A>& map, const K& key)
{
    return map.insert(typename std::map<K, T, C, A>::value_type(key, make_value(map))).first->second;
}

template <typename K, typename T, typename C, typename A>
inline
const T& mapped_at(const std::map<K, T, C, A>& map, const K& key)
{
    typename std::map<K, T, C, A>::const_iterator it = map.find(key);

    if (it == map.end())
        ElementNotFoundException(key);

    return it->second;
}


// enumerators
template <typename T>
class const_enumerator
{
public:
    explicit const_enumerator(const T& list)
        : it(list.begin()),
          end(list.end())
    {}

    bool more() const
    {
        return it != end;
    }

    typename T::const_reference
    next()
    {
        return *(it++);
    }

private:
    typename T::const_iterator it, end;
};

template <typename A>
class const_enumerator<std::vector<bool, A> >
{
public:
    explicit const_enumerator(const std::vector<bool, A>& list)
        : it(list.begin()),
          end(list.end())
    {}

    bool more() const
    {
        return it != end;
    }

    bool next()
    {
        return *(it++);
    }

private:
    typename std::vector<bool, A>::const_iterator it, end;
};

template <typename T>
class enumerator
{
public:
    explicit enumerator(T& list)
        : it(list.begin()),
          end(list.end())
    {}

    bool more() const
    {
        return it != end;
    }

    typename T::reference
    next()
    {
        return *(it++);
    }

private:
    typename T::iterator it, end;
};


template <typename K, typename V>
std::map<V, K> reverse_map(const std::map<K, V>& map)
{
    std::map<V, K> reversed;

    for (typename std::map<K, V>::const_iterator it = map.begin(); it != map.end(); ++it)
        reversed[it->second] = it->first;

    return reversed;
}

}
