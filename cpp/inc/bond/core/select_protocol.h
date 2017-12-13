// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "exception.h"
#include "protocol.h"
#include "runtime_schema.h"
#include "select_protocol_fwd.h"

#ifdef BOND_NO_CXX14_GENERIC_LAMBDAS
#include <functional>
#endif


namespace bond
{
namespace detail
{


// Overload of Apply used to extract bonded<T> from marshaled payload
template <typename Protocols, typename T, typename U, typename Reader>
inline bool
Apply(const boost::reference_wrapper<bonded<T> >& ref, const bonded<U, Reader>& value)
{
    value.template Deserialize<Protocols>(ref.get());
    return false;
}


template <typename Protocols, typename F>
inline auto TryEachProtocol(F&& f)
#ifdef BOND_NO_CXX14_RETURN_TYPE_DEDUCTION
    -> decltype(mpl::try_apply<typename Protocols::type>(std::forward<F>(f)))
#endif
{
    return mpl::try_apply<typename Protocols::type>(std::forward<F>(f));
}


template <typename T, typename Protocols>
struct NextProtocolFunctor
{
    template <typename Buffer, typename Transform, typename Reader>
    boost::optional<std::pair<ProtocolType, bool> >
    operator()(Buffer& input, const Transform& transform, const mpl::identity<Reader>&) const
    {
        Reader reader(input);

        if (reader.ReadVersion())
        {
            return std::make_pair(
                static_cast<ProtocolType>(Reader::magic),
                Apply<Protocols>(transform, bonded<T, ProtocolReader>(reader)));
        }

        return {};
    }

    template <typename Buffer, typename Transform, typename Reader>
    boost::optional<bool>
    operator()(Buffer& input, const Transform& transform, uint16_t protocol, const mpl::identity<Reader>&) const
    {
        if (Reader::magic == protocol)
        {
            Reader reader(input);
            return Apply<Protocols>(transform, bonded<T, Reader&>(reader));
        }

        return {};
    }


    template <template <typename Writer, typename ProtocolsT> class Transform>
    struct TransformFunctor
    {
        template <typename Buffer, typename Reader>
        boost::optional<bool>
        operator()(const T& value, Buffer& output, uint16_t protocol, const mpl::identity<Reader>&) const
        {
            if (Reader::magic == protocol)
            {
                using Writer = typename get_protocol_writer<Reader, Buffer>::type;

                Writer writer(output);
                return Apply<Protocols>(Transform<Writer, Protocols>(writer), value);
            }

            return {};
        }
    };
};


template <typename Protocols>
struct NextProtocolFunctor<void, Protocols>
{
    template <typename Buffer, typename Transform, typename Reader>
    boost::optional<std::pair<ProtocolType, bool> >
    operator()(const RuntimeSchema& schema, Buffer& input, const Transform& transform, const mpl::identity<Reader>&) const
    {
        Reader reader(input);

        if (reader.ReadVersion())
        {
            return std::make_pair(
                static_cast<ProtocolType>(Reader::magic),
                Apply<Protocols>(transform, bonded<void, ProtocolReader>(reader, schema)));
        }

        return {};
    }

    template <typename Buffer, typename Transform, typename Reader>
    boost::optional<bool>
    operator()(const RuntimeSchema& schema, Buffer& input, const Transform& transform, uint16_t protocol, const mpl::identity<Reader>&) const
    {
        if (Reader::magic == protocol)
        {
            Reader reader(input);
            return Apply<Protocols>(transform, bonded<void, Reader&>(reader, schema));
        }

        return {};
    }
};


// Select protocol and apply transform using compile-time schema
template <typename T, typename Protocols, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> NextProtocol(Buffer& input, const Transform& transform)
{
    auto&& visitor =
#ifndef BOND_NO_CXX14_GENERIC_LAMBDAS
        [&](const auto& identity) { return NextProtocolFunctor<T, Protocols>{}(input, transform, identity); };
#else
        std::bind(NextProtocolFunctor<T, Protocols>{}, std::ref(input), std::cref(transform), std::placeholders::_1);
#endif

    if (auto&& result = TryEachProtocol<typename Protocols::template FilterBuffer<Buffer> >(std::move(visitor)))
    {
        return result.get();
    }

    UnknownProtocolException();
}


// Select protocol and apply transform using runtime schema
template <typename Protocols, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> NextProtocol(const RuntimeSchema& schema, Buffer& input, const Transform& transform)
{
    auto&& visitor =
#ifndef BOND_NO_CXX14_GENERIC_LAMBDAS
        [&](const auto& identity) { return NextProtocolFunctor<void, Protocols>{}(schema, input, transform, identity); };
#else
        std::bind(NextProtocolFunctor<void, Protocols>{}, std::cref(schema), std::ref(input), std::cref(transform), std::placeholders::_1);
#endif

    if (auto&& result = TryEachProtocol<typename Protocols::template FilterBuffer<Buffer> >(std::move(visitor)))
    {
        return result.get();
    }

    UnknownProtocolException();
}


// Select protocol based on magic number and apply transform using compile-time schema
template <typename T, typename Protocols, typename Buffer, typename Transform>
inline bool NextProtocol(Buffer& input, const Transform& transform, uint16_t protocol)
{
    auto&& visitor =
#ifndef BOND_NO_CXX14_GENERIC_LAMBDAS
        [&](const auto& identity) { return NextProtocolFunctor<T, Protocols>{}(input, transform, protocol, identity); };
#else
        std::bind(NextProtocolFunctor<T, Protocols>{}, std::ref(input), std::cref(transform), protocol, std::placeholders::_1);
#endif

    if (auto&& result = TryEachProtocol<typename Protocols::template FilterBuffer<Buffer> >(std::move(visitor)))
    {
        return result.get();
    }

    UnknownProtocolException(protocol);
}


// Select protocol based on magic number and apply transform using runtime schema
template <typename Protocols, typename Buffer, typename Transform>
inline bool NextProtocol(const RuntimeSchema& schema, Buffer& input, const Transform& transform, uint16_t protocol)
{
    auto&& visitor =
#ifndef BOND_NO_CXX14_GENERIC_LAMBDAS
        [&](const auto& identity) { return NextProtocolFunctor<void, Protocols>{}(schema, input, transform, protocol, identity); };
#else
        std::bind(NextProtocolFunctor<void, Protocols>{}, std::cref(schema), std::ref(input), std::cref(transform), protocol, std::placeholders::_1);
#endif

    if (auto&& result = TryEachProtocol<typename Protocols::template FilterBuffer<Buffer> >(std::move(visitor)))
    {
        return result.get();
    }

    UnknownProtocolException(protocol);
}


// Select protocol based on magic number and apply instance of serializing transform
template <template <typename Writer, typename ProtocolsT> class Transform, typename Protocols, typename T, typename Buffer>
inline bool NextProtocol(const T& value, Buffer& output, uint16_t protocol)
{
    using TransformFunctor = typename NextProtocolFunctor<T, Protocols>::template TransformFunctor<Transform>;

    auto&& visitor =
#ifndef BOND_NO_CXX14_GENERIC_LAMBDAS
        [&](const auto& identity) { return TransformFunctor{}(value, output, protocol, identity); };
#else
        std::bind(TransformFunctor{}, std::cref(value), std::ref(output), protocol, std::placeholders::_1);
#endif

    if (auto&& result = TryEachProtocol<typename Protocols::FilterEnabled>(std::move(visitor)))
    {
        return result.get();
    }

    UnknownProtocolException(protocol);
}


} // namespace detail


//
// Apply transform to serialized data that was generated using Marshaler
//


// Use compile-time schema
template <typename T, typename Protocols, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> SelectProtocolAndApply(Buffer& input, const Transform& transform)
{
    return detail::NextProtocol<T, Protocols>(input, transform);
}


// Use runtime schema
template <typename Protocols, typename Buffer, typename Transform>
inline std::pair<ProtocolType, bool> SelectProtocolAndApply(
    const RuntimeSchema& schema,
    Buffer& input,
    const Transform& transform)
{
    return detail::NextProtocol<Protocols>(schema, input, transform);
}


// Apply deserializing transform with a protocol specified by magic number
// Use compile-time schema
template <typename T, typename Protocols = BuiltInProtocols, typename Transform, typename Buffer>
inline bool Apply(const Transform& transform, Buffer& input, uint16_t protocol)
{
    return detail::NextProtocol<T, Protocols>(input, transform, protocol);
}


// Use runtime schema
template <typename Protocols = BuiltInProtocols, typename Transform, typename Buffer>
inline bool Apply(const Transform& transform, const RuntimeSchema& schema, Buffer& input, uint16_t protocol)
{
    return detail::NextProtocol<Protocols>(schema, input, transform, protocol);
}


// Apply an instance of serializing transform with a protocol specified by magic number
template <template <typename Writer, typename ProtocolsT> class Transform, typename Protocols = BuiltInProtocols, typename T, typename Buffer>
inline bool Apply(const T& value, Buffer& output, uint16_t protocol)
{
    return detail::NextProtocol<Transform, Protocols>(value, output, protocol);
}


} // namespace bond


#ifdef BOND_LIB_TYPE
#if BOND_LIB_TYPE != BOND_LIB_TYPE_HEADER
#include "detail/select_protocol_extern.h"
#endif
#else
#error BOND_LIB_TYPE is undefined
#endif
