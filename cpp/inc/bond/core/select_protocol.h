// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "exception.h"
#include "protocol.h"
#include "runtime_schema.h"
#include "exception.h"

namespace bond
{
namespace detail
{


// Overload of Apply used to extract bonded<T> from marshaled payload
template <typename T, typename U, typename Reader>
inline bool
Apply(const boost::reference_wrapper<bonded<T> >& ref, const bonded<U, Reader>& value)
{
    value.Deserialize(ref.get());
    return false;
}


// Select protocol and apply transform using compile-time schema
template <typename T, typename Buffer, typename Transform, typename EndIter>
inline std::pair<ProtocolType, bool> NextProtocol(
    const EndIter&,
    const EndIter&,
    Buffer&,
    const Transform&)
{
    UnknownProtocolException();
    return std::make_pair(MARSHALED_PROTOCOL, false);
}


template <typename T, typename Buffer, typename Transform, typename Iter, typename EndIter>
inline std::pair<ProtocolType, bool> NextProtocol(
    const Iter&,
    const EndIter& end,
    Buffer& input,
    const Transform& transform)
{
    typedef typename boost::mpl::deref<Iter>::type Reader;

    Reader reader(input);

    if (reader.ReadVersion())
    {
        return std::make_pair(
            static_cast<ProtocolType>(Reader::magic), 
            Apply(transform, bonded<T, ProtocolReader<Buffer> >(reader)));
    }
    else
    {
        return NextProtocol<T>(typename boost::mpl::next<Iter>::type(), end, input, transform);
    }
}


// Select protocol and apply transform using runtime schema
template <typename Buffer, typename Transform, typename EndIter>
inline std::pair<ProtocolType, bool> NextProtocol(
    const EndIter&,
    const EndIter&,
    const RuntimeSchema&,
    Buffer&,
    const Transform&)
{
    UnknownProtocolException(); 
    return std::make_pair(MARSHALED_PROTOCOL, false);
}


template <typename Buffer, typename Transform, typename Iter, typename EndIter>
inline std::pair<ProtocolType, bool> NextProtocol(
    const Iter&,
    const EndIter& end,
    const RuntimeSchema& schema,
    Buffer& input,
    const Transform& transform)
{
    typedef typename boost::mpl::deref<Iter>::type Reader;

    Reader reader(input);

    if (reader.ReadVersion())
    {
        return std::make_pair(
            static_cast<ProtocolType>(Reader::magic), 
            Apply(transform, bonded<void, ProtocolReader<Buffer> >(reader, schema)));
    }
    else
    {
        return NextProtocol(typename boost::mpl::next<Iter>::type(), end, schema, input, transform);
    }
}


// Select protocol based on magic number and apply transform using compile-time schema
template <typename T, typename Buffer, typename Transform, typename EndIter>
inline bool NextProtocol(
    const EndIter&,
    const EndIter&,
    Buffer&,
    const Transform&, uint16_t protocol)
{
    UnknownProtocolException(protocol);
    return false; 
}


template <typename T, typename Buffer, typename Transform, typename Iter, typename EndIter>
inline bool NextProtocol(
    const Iter&,
    const EndIter& end,
    Buffer& input,
    const Transform& transform,
    uint16_t protocol)
{
    typedef typename boost::mpl::deref<Iter>::type Reader;

    if (Reader::magic == protocol)
    {
        Reader reader(input);
        return Apply(transform, bonded<T, Reader&>(reader));
    }
    else
    {
        return NextProtocol<T>(typename boost::mpl::next<Iter>::type(), end, input, transform, protocol);
    }
}


// Select protocol based on magic number and apply transform using runtime schema
template <typename Buffer, typename Transform, typename EndIter>
inline bool NextProtocol(
    const EndIter&,
    const EndIter&,
    const RuntimeSchema&, Buffer&, const Transform&, uint16_t protocol)
{
    UnknownProtocolException(protocol); 
    return false;
}


template <typename Buffer, typename Transform, typename Iter, typename EndIter>
inline bool NextProtocol(
    const Iter&,
    const EndIter& end,
    const RuntimeSchema& schema,
    Buffer& input,
    const Transform& transform,
    uint16_t protocol)
{
    typedef typename boost::mpl::deref<Iter>::type Reader;

    if (Reader::magic == protocol)
    {
        Reader reader(input);
        return Apply(transform, bonded<void, Reader&>(reader, schema));
    }
    else
    {
        return NextProtocol(typename boost::mpl::next<Iter>::type(), end, schema, input, transform, protocol);
    }
}


// Select protocol based on magic number and apply instance of serializing transform 
template <template <typename Writer> class Transform, typename Buffer, typename T, typename EndIter>
inline bool NextProtocol(
    const EndIter&,
    const EndIter&,
    const T&,
    Buffer&,
    uint16_t protocol)
{
    UnknownProtocolException(protocol);
    return false;
}


template <template <typename Writer> class Transform, typename Buffer, typename T, typename Iter, typename EndIter>
inline bool NextProtocol(
    const Iter&,
    const EndIter& end,
    const T& value,
    Buffer& output,
    uint16_t protocol)
{
    typedef typename boost::mpl::deref<Iter>::type Reader;

    if (Reader::magic == protocol)
    {
        typename Reader::Writer writer(output);
        return Apply(Transform<typename Reader::Writer>(writer), value);
    }
    else
    {
        return NextProtocol<Transform>(typename boost::mpl::next<Iter>::type(), end, value, output, protocol);
    }
}


} // namespace detail


//
// Apply transform to serialized data that was generated using Marshaler 
//


// Use compile-time schema
template <typename T, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> SelectProtocolAndApply(
    Buffer& input,
    const Transform& transform)
{
    return detail::NextProtocol<T>(typename Protocols<Buffer>::begin(), typename Protocols<Buffer>::end(), input, transform);
}


// Use runtime schema
template <typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> SelectProtocolAndApply(
    const RuntimeSchema& schema,
    Buffer& input,
    const Transform& transform)
{
    return detail::NextProtocol(typename Protocols<Buffer>::begin(), typename Protocols<Buffer>::end(), schema, input, transform);
}


// Apply deserializing transform with a protocol specified by magic number 
// Use compile-time schema
template <typename T, typename Buffer, typename Transform>
inline bool Apply(
    const Transform& transform,
    Buffer& input,
    uint16_t protocol)
{
    return detail::NextProtocol<T>(
        typename Protocols<Buffer>::begin(),
        typename Protocols<Buffer>::end(),
        input,
        transform,
        protocol
    );
}


// Use runtime schema
template <typename Buffer, typename Transform>
inline bool Apply(
    const Transform& transform,
    const RuntimeSchema& schema,
    Buffer& input,
    uint16_t protocol)
{
    return detail::NextProtocol(
        typename Protocols<Buffer>::begin(),
        typename Protocols<Buffer>::end(),
        schema,
        input,
        transform,
        protocol
    );
}


// Apply an instance of serializing transform with a protocol specified by magic number
template <template <typename Writer> class Transform, typename Buffer, typename T>
inline bool Apply(const T& value, Buffer& output, uint16_t protocol)
{
    return detail::NextProtocol<Transform>(
        typename Protocols<Buffer>::begin(),
        typename Protocols<Buffer>::end(),
        value,
        output,
        protocol);
}

} // namespace bond

