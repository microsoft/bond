// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "customize.h"
#include "detail/any.h"
#include "detail/mpl.h"
#include "detail/odr.h"
#include "detail/visit_any.h"
#include "traits.h"

#include <bond/protocol/compact_binary.h>
#include <bond/protocol/fast_binary.h>
#include <bond/protocol/simple_binary.h>
#include <bond/protocol/simple_json_reader.h>
#include <bond/stream/input_buffer.h>

#include <boost/make_shared.hpp>
#include <boost/optional.hpp>
#include <boost/ref.hpp>


namespace bond
{

#ifdef BOND_COMPACT_BINARY_PROTOCOL
template <typename Buffer> struct
is_protocol_enabled<CompactBinaryReader<Buffer> >
    : std::true_type {};
#endif

#ifdef BOND_SIMPLE_BINARY_PROTOCOL
template <typename Buffer, typename MarshaledBondedProtocols> struct
is_protocol_enabled<SimpleBinaryReader<Buffer, MarshaledBondedProtocols> >
    : std::true_type {};
#endif

#ifdef BOND_SIMPLE_JSON_PROTOCOL
template <typename Buffer> struct
is_protocol_enabled<SimpleJsonReader<Buffer> >
    : std::true_type {};
#endif

#ifdef BOND_FAST_BINARY_PROTOCOL
template <typename Buffer> struct
is_protocol_enabled<FastBinaryReader<Buffer> >
    : std::true_type {};
#endif

// uses_dynamic_parser
template <typename Reader, typename Enable = void> struct
uses_dynamic_parser
    : std::false_type {};

template <typename Reader> struct
uses_dynamic_parser<Reader, typename boost::enable_if<
    std::is_same<typename Reader::Parser, DynamicParser<Reader&> > >::type>
    : std::true_type {};

template <typename Reader> struct
uses_dynamic_parser<Reader&>
    : uses_dynamic_parser<Reader> {};

// uses_dom_parser
template <typename Reader, typename Enable = void> struct
uses_dom_parser
    : std::false_type {};

template <typename Reader> struct
uses_dom_parser<Reader, typename boost::enable_if<
    std::is_same<typename Reader::Parser, DOMParser<Reader&> > >::type>
    : std::true_type {};

template <typename Reader> struct
uses_dom_parser<Reader&>
    : uses_dom_parser<Reader> {};


template <typename Reader, typename Unused> struct
uses_marshaled_bonded
    : uses_static_parser<Reader> {};


template <typename... T>
struct Protocols
{
private:
    template <typename Buffer>
    struct FilterBufferHelper;

public:
    using type = detail::mpl::list<T...>;

    template <typename... U>
    using Append = typename Protocols<detail::mpl::append_t<type, U...> >::type;

    using FilterEnabled = typename Protocols<detail::mpl::filter_t<type, is_protocol_enabled> >::type;

    template <typename Buffer>
    using FilterBuffer = typename FilterBufferHelper<Buffer>::type;

private:
    template <typename Buffer>
    struct FilterBufferHelper
    {
        template <typename U>
        using check_buffer = std::is_same<typename std::remove_reference<typename U::Buffer>::type, Buffer>;

        using type = typename Protocols<detail::mpl::filter_t<typename FilterEnabled::type, check_buffer> >::type;
    };
};


template <typename... T>
struct Protocols<detail::mpl::list<T...> >
{
    using type = Protocols<T...>;
};


// Deriving from Protocols<> instead of using an alias to avoid
// binary size increase due to much longer type/function names on VC.
struct BuiltInProtocols
    : Protocols<
        CompactBinaryReader<InputBuffer>,
        SimpleBinaryReader<InputBuffer>,
        FastBinaryReader<InputBuffer>,
        SimpleJsonReader<InputBuffer> > {};


struct ValueReader
{
    BOOST_STATIC_CONSTEXPR uint16_t magic = 0x5256 /*VR*/;
    using Buffer = void;

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


BOND_DEFINE_BUFFER_MAGIC(ValueReader::Buffer, 0);


namespace detail
{
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4296) // C4296: '<' : expression is always false
#endif // _MSC_VER

    // Avoid std::max due to a bug in Visual C++ 2017.
    template <std::size_t V0, std::size_t V1>
    struct max_of
        : std::integral_constant<std::size_t, (V0 < V1 ? V1 : V0)> {};

#ifdef _MSC_VER
#pragma warning(pop)
#endif // _MSC_VER

    template <typename List> struct
    max_size;

    template <> struct
    max_size<detail::mpl::list<> >
        : std::integral_constant<std::size_t, 0> {};

    template <typename T, typename... U> struct
    max_size<detail::mpl::list<T, U...> >
        : max_of<sizeof(T), max_size<detail::mpl::list<U...> >::value> {};

    using protocol_max_size = max_size<BuiltInProtocols::Append<ValueReader>::type>;

} // namespace detail


class ProtocolReader
{
public:
    using Parser = void;
    using Writer = void;

    ProtocolReader(const ValueReader& reader = {})
        : _value(reader)
    {}

    template <typename Reader, typename boost::enable_if<is_reader<Reader> >::type* = nullptr>
    ProtocolReader(const Reader& reader)
        : _value(reader)
    {}

    bool operator==(const ProtocolReader& rhs) const
    {
        return _value == rhs._value;
    }

#if !defined(BOND_NO_CXX14_RETURN_TYPE_DEDUCTION) && !defined(BOND_NO_CXX14_GENERIC_LAMBDAS)
    template <typename Protocols, typename Visitor>
    auto Visit(Visitor&& visitor)
#else
    template <typename Protocols, typename Result, typename Visitor>
    typename detail::visitor_result<Result>::type Visit(Visitor&& visitor)
#endif
    {
        return detail::visit_any<typename Protocols::template Append<ValueReader>::type
#if defined(BOND_NO_CXX14_RETURN_TYPE_DEDUCTION) || defined(BOND_NO_CXX14_GENERIC_LAMBDAS)
            , Result
#endif
            >(std::forward<Visitor>(visitor), _value);
    }

private:
    template <typename Reader> struct
    reader_id
        : std::integral_constant<uint32_t, Reader::magic | (buffer_magic<typename Reader::Buffer>::value << 16)> {};


    detail::any<reader_id, detail::protocol_max_size::value> _value;
};


} // namespace bond
