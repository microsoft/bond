// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "config.h"
#include "scalar_interface.h"
#include <boost/utility/enable_if.hpp>

#ifndef BOND_NO_CXX11_HDR_TYPE_TRAITS
#   include <type_traits>
#   define BOND_TYPE_TRAITS_NAMESPACE ::std
#else
#   include <boost/type_traits.hpp>
#   define BOND_TYPE_TRAITS_NAMESPACE ::boost
#endif

namespace bond
{

using BOND_TYPE_TRAITS_NAMESPACE::false_type;
using BOND_TYPE_TRAITS_NAMESPACE::is_arithmetic;
using BOND_TYPE_TRAITS_NAMESPACE::is_base_of;
using BOND_TYPE_TRAITS_NAMESPACE::is_class;
using BOND_TYPE_TRAITS_NAMESPACE::is_enum;
using BOND_TYPE_TRAITS_NAMESPACE::is_floating_point;
using BOND_TYPE_TRAITS_NAMESPACE::is_integral;
using BOND_TYPE_TRAITS_NAMESPACE::is_object;
using BOND_TYPE_TRAITS_NAMESPACE::is_pod;
using BOND_TYPE_TRAITS_NAMESPACE::is_reference;
using BOND_TYPE_TRAITS_NAMESPACE::is_same;
using BOND_TYPE_TRAITS_NAMESPACE::is_signed;
using BOND_TYPE_TRAITS_NAMESPACE::is_unsigned;
using BOND_TYPE_TRAITS_NAMESPACE::is_void;
using BOND_TYPE_TRAITS_NAMESPACE::make_signed;
using BOND_TYPE_TRAITS_NAMESPACE::make_unsigned;
using BOND_TYPE_TRAITS_NAMESPACE::remove_const;
using BOND_TYPE_TRAITS_NAMESPACE::remove_reference;
using BOND_TYPE_TRAITS_NAMESPACE::true_type;

#undef BOND_TYPE_TRAITS_NAMESPACE

// is_signed_int
template <typename T> struct
is_signed_int
{
    static const bool value = is_signed<T>::value
                          && !is_floating_point<T>::value
                          && !is_enum<T>::value;
};


// is_signed_int_or_enum
template <typename T> struct
is_signed_int_or_enum
{
    static const bool value = is_signed_int<T>::value
                           || is_enum<T>::value;
};


// schema
template <typename T, typename Enable = void> struct
schema;

template <typename T> struct
schema<T, typename boost::enable_if<is_class<typename T::Schema::fields> >::type>
{
    typedef typename T::Schema type;
};


// has_schema
template <typename T, typename Enable = void> struct
has_schema
    : false_type {};


template <typename T> struct
has_schema<T, typename boost::enable_if<is_class<typename schema<T>::type> >::type>
    : true_type {};


// is_protocol_same
template <typename Reader, typename Writer> struct
is_protocol_same
    : false_type {};


template <template <typename T> class Reader, typename I, template <typename T> class Writer, typename O> struct
is_protocol_same<Reader<I>, Writer<O> >
    : is_same<typename Reader<O>::Writer, Writer<O> > {};

// For protocols that have multiple versions, specialize this template
template <typename Reader> struct
protocol_has_multiple_versions 
    : false_type {};


// ... and overload this function. 
template <typename Reader, typename Writer>
inline 
bool is_protocol_version_same(const Reader&, const Writer&)
{
    return true;
}


// By default if a protocol has multiple versions any of the versions can be
// used by an application. This template can be specialized to fix protocol to
// a single version specified by default_version<Reader>. This can enable some
// optimizations, e.g. fast pass-through w/o checking version at runtime.
template <typename Reader> struct
enable_protocol_versions
    : true_type {};


// get_protocol_writer
template <typename Reader, typename OutputStream> struct
get_protocol_writer;


template <template <typename T> class Reader, typename I, typename OutputStream> struct
get_protocol_writer<Reader<I>, OutputStream>
{
    typedef typename Reader<OutputStream>::Writer type;
};


template <typename T, T> struct 
check_method
    : true_type {};


template <typename Reader, typename Unused = void> struct 
uses_marshaled_bonded;


// is_type_alias
template <typename T> struct
is_type_alias
    : is_object<typename aliased_type<T>::type> {};


} // namespace bond

