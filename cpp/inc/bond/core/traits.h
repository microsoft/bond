// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bond_fwd.h"
#include "detail/mpl.h"
#include "scalar_interface.h"

#include <boost/static_assert.hpp>
#include <boost/utility/enable_if.hpp>

#include <type_traits>


namespace bond
{

// is_signed_int
template <typename T> struct
is_signed_int
    : std::integral_constant<bool,
        std::is_signed<T>::value
        && !std::is_floating_point<T>::value
        && !std::is_enum<T>::value> {};


// is_signed_int_or_enum
template <typename T> struct
is_signed_int_or_enum
    : std::integral_constant<bool,
        is_signed_int<T>::value
        || std::is_enum<T>::value> {};


// schema
template <typename T, typename Enable = void> struct
schema;

template <typename T> struct
schema<T, typename boost::enable_if<std::is_class<typename T::Schema::fields> >::type>
{
    typedef typename T::Schema type;
};


// has_schema
template <typename T, typename Enable = void> struct
has_schema
    : std::false_type {};


template <typename T> struct
has_schema<T, typename boost::enable_if<std::is_class<typename schema<T>::type> >::type>
    : std::true_type {};


// is_protocol_same
template <typename Reader, typename Writer> struct
is_protocol_same
    : std::false_type {};


template <template <typename T> class Reader, typename Input, template <typename T> class Writer, typename Output> struct
is_protocol_same<Reader<Input>, Writer<Output> >
    : std::is_same<typename Reader<Output>::Writer, Writer<Output> > {};


template <template <typename T, typename U> class Reader, typename Input, typename MarshaledBondedProtocols, template <typename T> class Writer, typename Output> struct
is_protocol_same<Reader<Input, MarshaledBondedProtocols>, Writer<Output> >
    : std::is_same<typename Reader<Output, MarshaledBondedProtocols>::Writer, Writer<Output> > {};


// For protocols that have multiple versions, specialize this template...
template <typename Reader> struct
protocol_has_multiple_versions
    : std::false_type {};


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
    : std::true_type {};


// get_protocol_writer
template <typename Reader, typename Output> struct
get_protocol_writer;

template <template <typename T> class Reader, typename I, typename Output> struct
get_protocol_writer<Reader<I>, Output>
{
    typedef typename Reader<Output>::Writer type;
};

template <template <typename T, typename U> class Reader, typename Input, typename MarshaledBondedProtocols, typename Output> struct
get_protocol_writer<Reader<Input, MarshaledBondedProtocols>, Output>
{
    typedef typename Reader<Output, MarshaledBondedProtocols>::Writer type;
};


// Used only for older compilers for which BOND_NO_SFINAE_EXPR is defined.
template <typename T, T> struct
check_method
    : std::true_type {};


template <typename T> struct
is_bonded
    : std::false_type {};


template <typename T, typename Reader> struct
is_bonded<bonded<T, Reader> >
    : std::true_type {};


template <typename Reader, typename Unused = void> struct
uses_marshaled_bonded;


// is_type_alias
template <typename T> struct
is_type_alias
    : std::is_object<typename aliased_type<T>::type> {};


// is_reader
template <typename Input, typename T = void, typename Enable = void> struct
is_reader
    : std::false_type {};

template <typename Input, typename T> struct
is_reader<Input&, T>
    : is_reader<Input, T> {};

template <typename Input, typename T> struct
is_reader<Input, T, typename boost::enable_if<std::is_class<typename Input::Parser> >::type>
    : std::true_type {};


template <typename T> struct
buffer_magic
{
    BOOST_STATIC_ASSERT_MSG(
        detail::mpl::always_false<T>::value,
        "buffer_magic is undefined for this buffer. Make sure buffer_magic is specialized for this buffer type.");
};

template <typename T> struct
buffer_magic<T&>
    : buffer_magic<T> {};


template <uint16_t Id> struct
unique_buffer_magic_check;

#define BOND_DEFINE_BUFFER_MAGIC(Buffer, Id) \
    template <> struct unique_buffer_magic_check<Id> {}; \
    template <> struct buffer_magic<Buffer> : std::integral_constant<uint16_t, Id> {}

// uses_static_parser
template <typename Reader, typename Enable = void> struct
uses_static_parser
    : std::false_type {};

template <typename Reader> struct
uses_static_parser<Reader, typename boost::enable_if<
    std::is_same<typename Reader::Parser, StaticParser<Reader&> > >::type>
    : std::true_type {};

template <typename Reader> struct
uses_static_parser<Reader&>
    : uses_static_parser<Reader> {};

} // namespace bond
