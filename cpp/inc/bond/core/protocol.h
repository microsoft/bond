// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/make_shared.hpp>
#include <boost/variant.hpp>
#include <boost/ref.hpp>
#include <boost/mpl/list.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/copy_if.hpp>

#include "customize.h"
#include "detail/odr.h"
#include <bond/protocol/simple_binary.h>
#include <bond/protocol/compact_binary.h>
#include <bond/protocol/fast_binary.h>
#include <bond/protocol/simple_json_reader.h>

#if !defined(BOND_COMPACT_BINARY_PROTOCOL) \
 && !defined(BOND_SIMPLE_BINARY_PROTOCOL) \
 && !defined(BOND_FAST_BINARY_PROTOCOL) \
 && !defined(BOND_SIMPLE_JSON_PROTOCOL)

#   define BOND_COMPACT_BINARY_PROTOCOL
#   define BOND_SIMPLE_BINARY_PROTOCOL
#   define BOND_FAST_BINARY_PROTOCOL
// BOND_SIMPLE_JSON_PROTOCOL disabled by default

#endif

namespace bond
{

#ifdef BOND_COMPACT_BINARY_PROTOCOL
template <typename Buffer> struct 
is_protocol_enabled<CompactBinaryReader<Buffer> > 
    : true_type {};
#endif 

#ifdef BOND_SIMPLE_BINARY_PROTOCOL
template <typename Buffer> struct 
is_protocol_enabled<SimpleBinaryReader<Buffer> > 
    : true_type {};
#endif 

#ifdef BOND_SIMPLE_JSON_PROTOCOL
template <typename Buffer> struct 
is_protocol_enabled<SimpleJsonReader<Buffer> > 
    : true_type {};
#endif 

#ifdef BOND_FAST_BINARY_PROTOCOL
template <typename Buffer> struct 
is_protocol_enabled<FastBinaryReader<Buffer> > 
    : true_type {};
#endif 

// uses_static_parser
template <typename Reader, typename Enable = void> struct
uses_static_parser
    : false_type {};

template <typename Reader> struct
uses_static_parser<Reader, typename boost::enable_if<
    is_same<typename Reader::Parser, StaticParser<Reader&> > >::type>
    : true_type {};

template <typename Reader> struct
uses_static_parser<Reader&>
    : uses_static_parser<Reader> {};

// uses_dynamic_parser
template <typename Reader, typename Enable = void> struct
uses_dynamic_parser
    : false_type {};

template <typename Reader> struct
uses_dynamic_parser<Reader, typename boost::enable_if<
    is_same<typename Reader::Parser, DynamicParser<Reader&> > >::type>
    : true_type {};

template <typename Reader> struct
uses_dynamic_parser<Reader&>
    : uses_dynamic_parser<Reader> {};

// uses_dom_parser
template <typename Reader, typename Enable = void> struct
uses_dom_parser
    : false_type {};

template <typename Reader> struct
uses_dom_parser<Reader, typename boost::enable_if<
    is_same<typename Reader::Parser, DOMParser<Reader&> > >::type>
    : true_type {};

template <typename Reader> struct
uses_dom_parser<Reader&>
    : uses_dom_parser<Reader> {};


template <typename Reader, typename Unused> struct 
uses_marshaled_bonded
    : uses_static_parser<Reader> {};


struct ValueReader
{
    // Constructors that explicitly declared noexcept are needed for
    // boost::variant to use optimized code path. 
    ValueReader() BOND_NOEXCEPT
        : pointer(NULL)
    {}

    template <typename U>
    ValueReader(boost::reference_wrapper<U> value) BOND_NOEXCEPT
        : pointer(&static_cast<const U&>(value))
    {}

    template <typename U>
    ValueReader(const U& value)
        : instance(boost::make_shared<U>(value)),
          pointer(instance.get())
    {}

    template <typename U>
    ValueReader(boost::shared_ptr<U> value) BOND_NOEXCEPT
        : instance(boost::static_pointer_cast<const void>(value)),
          pointer(instance.get())
    {}
    
    ValueReader(const ValueReader& value) BOND_NOEXCEPT
        : instance(value.instance),
          pointer(value.pointer)
    {}

    bool operator==(const ValueReader& rhs) const
    {
        return instance == rhs.instance
            && pointer == rhs.pointer;
    }

    boost::shared_ptr<const void> instance;
    const void* pointer;
};


using boost::mpl::_;

template <typename Buffer>
struct Protocols
{
    typedef typename boost::mpl::list<
       CompactBinaryReader<Buffer>,
       SimpleBinaryReader<Buffer>,
       FastBinaryReader<Buffer>,
       SimpleJsonReader<Buffer>
    >::type built_in;

    typedef typename customize<protocols>::modify<built_in>::type all;
    
    typedef typename boost::mpl::copy_if<
        all, 
        is_protocol_enabled<_>, 
        boost::mpl::front_inserter<boost::mpl::list<> > >::type type;

    typedef typename boost::mpl::begin<type>::type begin;
};


template <typename Buffer>
struct ProtocolReader
{
    typedef void Parser;
    typedef void Writer;

    ProtocolReader()
        : value()
    {
        // Validate that all compilation units in a program use the same set of protocols.
        (void)one_definition<Protocols<Buffer>, typename Protocols<Buffer>::all>::value;
    }
    
    ProtocolReader(const ValueReader& x)
        : value(x)
    {}
    
    template <typename Reader>
    ProtocolReader(const Reader& reader)
        : value(reader)
    {}
    
    ProtocolReader(const ProtocolReader& that)
        : value(that.value)
    {}

    bool operator==(const ProtocolReader& rhs) const 
    {
        return value == rhs.value;
    }
    
    typename boost::make_variant_over<
        typename boost::mpl::push_front<
            typename Protocols<Buffer>::all, 
            ValueReader
        >::type
    >::type value;
};


} // namespace bond
