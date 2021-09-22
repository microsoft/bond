// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "pass_through.h"
#include "tags.h"

#include <bond/core/customize.h>
#include <bond/core/null.h>
#include <bond/core/protocol.h>
#include <bond/core/traits.h>
#include <bond/stream/input_buffer.h>
#include <bond/stream/output_buffer.h>

#include <cstddef>


namespace bond
{

class RuntimeSchema;
struct SchemaReader;

template <typename Writer, typename Protocols>
class Serializer;

namespace detail
{

// Visitor which applies protocol's parser to specified transform and data.
// It is used to dispatch to appropriate protocol at runtime.
template <typename T, typename Schema, typename Transform>
class _Parser
    : boost::noncopyable
{
public:
    _Parser(const Transform& transform, const Schema& schema)
        : _transform(transform),
          _schema(schema)
    {}


    template <typename Reader>
    typename boost::enable_if<is_protocol_enabled<typename std::remove_const<Reader>::type>, bool>::type
    operator()(Reader& reader) const
    {
        // Apply transform to serialized data
        return Apply(_transform, reader, _schema, false);
    }

    template <typename Reader>
    typename boost::disable_if<is_protocol_enabled<typename std::remove_const<Reader>::type>, bool>::type
    operator()(Reader& /*reader*/) const
    {
        // Don't instantiate deserialization code for disabled protocol to speed up build
        BOOST_ASSERT(false);
        return false;
    }

    template <typename Reader, typename Writer, typename Protocols>
    static
    typename boost::enable_if_c<is_protocol_same<Reader, Writer>::value
                             && protocol_has_multiple_versions<Reader>::value, bool>::type
    Apply(const Serializer<Writer, Protocols>& transform, Reader& reader, const Schema& schema, bool base)
    {
        if (is_protocol_version_same(reader, transform._output))
            return FastPassThrough(reader, transform._output, schema);
        else
            return typename Reader::Parser(reader, base).Apply(transform, schema);
    }

    template <typename Reader, typename Writer, typename Protocols>
    static
    typename boost::enable_if_c<is_protocol_same<Reader, Writer>::value
                             && !protocol_has_multiple_versions<Reader>::value, bool>::type
    Apply(const Serializer<Writer, Protocols>& transform, Reader& reader, const Schema& schema, bool base)
    {
        BOOST_VERIFY(!base);
        // Triggering the following assert means that bond::enable_protocol_versions trait is
        // defined/specialized inconsistently for the protocol in different compilation units.
        BOOST_ASSERT(is_protocol_version_same(reader, transform._output));
        return FastPassThrough(reader, transform._output, schema);
    }

    template <typename TransformT, typename Reader>
    static
    bool Apply(const TransformT& transform, Reader& reader, const Schema& schema, bool base)
    {
        return typename Reader::Parser(reader, base).Apply(transform, schema);
    }

protected:
    template <typename Reader, typename Writer, typename SchemaT>
    static
    bool FastPassThrough(Reader& reader, Writer& writer, const SchemaT&)
    {
        bonded<T, Reader&> value(reader);
        PassThrough(value, reader, writer);
        return false;
    }

    template <typename Reader, typename Writer>
    static
    bool FastPassThrough(Reader& reader, Writer& writer, const RuntimeSchema& schema)
    {
        bonded<void, Reader&> value(reader, schema);
        PassThrough(value, reader, writer);
        return false;
    }

    const Transform& _transform;
    const Schema&    _schema;
};


template <typename T, typename Schema, typename Transform, typename Enable = void>
class Parser
    : public _Parser<T, Schema, Transform>
{
public:
    Parser(const Transform& transform, const Schema& schema)
        : _Parser<T, Schema, Transform>(transform, schema)
    {}

    using _Parser<T, Schema, Transform>::operator();

    bool operator()(ValueReader& value) const
    {
        // "De-serializing" bonded<T> containing a non-serialized instance of T
        BOOST_VERIFY(value.pointer == NULL);
        return false;
    }
};


template <typename T, typename Schema, typename Transform>
class Parser<T, Schema, Transform, typename boost::enable_if_c<is_serializing_transform<Transform>::value && !std::is_same<T, void>::value>::type>
    : public _Parser<T, Schema, Transform>
{
public:
    Parser(const Transform& transform, const Schema& schema)
        : _Parser<T, Schema, Transform>(transform, schema)
    {}

    using _Parser<T, Schema, Transform>::operator();

    bool operator()(ValueReader& value) const
    {
        // Serializing bonded<T> containing a non-serialized instance of T
        BOOST_ASSERT(value.pointer);
        // NOTE TO USER: following assert may indicate that the generated file
        // _reflection.h was not included in compilation unit where T is serialized.
        BOOST_ASSERT(has_schema<T>::value);
        return StaticParser<const T&>(*static_cast<const T*>(value.pointer)).Apply(_transform, typename schema_for_passthrough<T>::type());
    }

protected:
    using _Parser<T, Schema, Transform>::_transform;
};


template <typename Reader, typename T>
inline void Skip(Reader& reader, const bonded<T, Reader&>& bonded)
{
    reader.Skip(bonded);
}


template <typename T, template <typename BufferT, typename MarshaledBondedProtocolsT> class Reader, typename Buffer, typename MarshaledBondedProtocols>
inline void Skip(const bonded<T, Reader<Buffer, MarshaledBondedProtocols>&>& bonded)
{
    // Skip the structure field-by-field by applying Null transform
    Apply<MarshaledBondedProtocols>(Null(), bonded);
}


template <typename Reader, typename T>
BOND_NO_INLINE void Skip(Reader& reader, const bonded<T, Reader&>& bonded, const std::nothrow_t&) BOND_NOEXCEPT
{
    try
    {
        Skip(reader, bonded);
    }
    catch(...)
    {}
}

template <typename T>
void Skip(SchemaReader&, const bonded<T, SchemaReader&>&, const std::nothrow_t&) BOND_NOEXCEPT
{}


template <typename T>
inline void Skip(ProtocolReader& /*reader*/, const bonded<T>& /*bonded*/) BOND_NOEXCEPT
{
    // Not skipping for outer structures
}


template <typename T>
inline void Skip(ProtocolReader& /*reader*/, const bonded<T>& /*bonded*/, const std::nothrow_t&) BOND_NOEXCEPT
{
    // Not skipping for outer structures
}


template <typename T, typename Protocols, typename Transform, typename Reader, typename Schema>
inline bool Parse(const Transform& transform, Reader& reader, const Schema& schema, const RuntimeSchema* runtime_schema, bool base)
{
    BOOST_VERIFY(!runtime_schema);
    return Parser<T, Schema, Transform>::Apply(transform, reader, schema, base);
}

template <typename T, typename Protocols, typename Transform, typename Reader, typename Schema>
inline bool Parse(const Transform& transform, Reader& reader, const Schema& schema, std::nullptr_t, bool base)
{
    return Parser<T, Schema, Transform>::Apply(transform, reader, schema, base);
}

template <typename T, typename Protocols, typename Transform, typename Schema>
inline bool Parse(const Transform& transform, ProtocolReader& reader, const Schema& schema)
{
    // Use named variable to avoid gcc silently copying objects (which
    // causes build break, because Parser<> is non-copyable).
    Parser<T, Schema, Transform> parser(transform, schema);

    if (auto&& result = reader.template Visit<Protocols
#if defined(BOND_NO_CXX14_RETURN_TYPE_DEDUCTION) || defined(BOND_NO_CXX14_GENERIC_LAMBDAS)
        , bool
#endif
        >(parser))
    {
        return result.get();
    }

    UnknownProtocolException();
}

template <typename T, typename Protocols, typename Transform, typename Schema>
inline bool Parse(const Transform& transform, ProtocolReader reader, const Schema& schema, const RuntimeSchema* runtime_schema, bool base)
{
    BOOST_VERIFY(!base);

    if (runtime_schema)
    {
        return Parse<void, Protocols>(transform, reader, *runtime_schema);
    }
    else
    {
        return Parse<T, Protocols>(transform, reader, schema);
    }
}

template <typename T, typename Protocols, typename Transform, typename Schema>
inline bool Parse(const Transform& transform, ProtocolReader reader, const Schema& schema, std::nullptr_t, bool base)
{
    BOOST_VERIFY(!base);
    return Parse<T, Protocols>(transform, reader, schema);
}


// Visitor which updates in-situ bonded<T> payload by merging it with an object.
template <typename T, typename Buffer>
class InsituMerge
    : boost::noncopyable
{
public:
    InsituMerge(const T& var, ProtocolReader& reader)
        : _var(var),
          _reader(reader)
    {}


    template <typename Reader>
    typename boost::enable_if<is_protocol_enabled<typename std::remove_const<Reader>::type> >::type
    operator()(Reader& reader) const
    {
        Buffer merged;
        typename get_protocol_writer<Reader, Buffer>::type writer(merged);

        Merge(_var, reader, writer);

        _reader = Reader(merged.GetBuffer());
    }

    template <typename Reader>
    typename boost::disable_if<is_protocol_enabled<typename std::remove_const<Reader>::type> >::type
    operator()(Reader& /*reader*/) const
    {
        // Don't instantiate code for disabled protocol to speed up build
        BOOST_ASSERT(false);
    }


    void operator()(ValueReader&) const
    {
        // Merge is undefined for non-serialized instance of T
        BOOST_VERIFY(false);
    }

private:
    const T& _var;
    ProtocolReader& _reader;
};


template <typename Protocols, typename Buffer = OutputBuffer, typename T>
inline void Merge(const T& var, ProtocolReader& reader)
{
    if (!reader.template Visit<Protocols
#if defined(BOND_NO_CXX14_RETURN_TYPE_DEDUCTION) || defined(BOND_NO_CXX14_GENERIC_LAMBDAS)
        , void
#endif
        >(InsituMerge<T, Buffer>(var, reader)))
    {
        UnknownProtocolException();
    }
}


} // namespace detail

} // namespace bond
