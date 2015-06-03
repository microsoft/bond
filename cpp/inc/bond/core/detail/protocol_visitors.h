// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "tags.h"
#include "pass_through.h"
#include <bond/stream/input_buffer.h>
#include <bond/stream/output_buffer.h>
#include <bond/core/traits.h>

namespace bond
{

class RuntimeSchema;

template <typename Writer>
class Serializer;

namespace detail
{

// Visitor which applies protocol's parser to specified transform and data.
// It is used to dispatch to appropriate protocol at runtime.
template <typename T, typename Schema, typename Transform>
class _Parser
    : public boost::static_visitor<bool>,
      boost::noncopyable
{
public:
    _Parser(const Transform& transform, const Schema& schema)
        : _transform(transform),
          _schema(schema)
    {}

    
    template <typename Reader>
    typename boost::enable_if<is_protocol_enabled<typename remove_const<Reader>::type>, bool>::type
    operator()(Reader& reader) const
    {
        // Apply transform to serialized data 
        return Apply(_transform, reader, _schema, false);
    }

    template <typename Reader>
    typename boost::disable_if<is_protocol_enabled<typename remove_const<Reader>::type>, bool>::type
    operator()(Reader& /*reader*/) const
    {
        // Don't instantiate deserialization code for disabled protocol to speed up build
        BOOST_ASSERT(false);
        return false;
    }

    template <template <typename U> class Reader, typename Writer>
    static
    typename boost::enable_if_c<is_protocol_same<Reader<InputBuffer>, Writer>::value
                             && protocol_has_multiple_versions<Reader<InputBuffer> >::value, bool>::type
    Apply(const Serializer<Writer>& transform, Reader<InputBuffer>& reader, const Schema& schema, bool base)
    {
        if (is_protocol_version_same(reader, transform._output))
            return FastPassThrough(reader, transform._output, schema);
        else
            return typename Reader<InputBuffer>::Parser(reader, base).Apply(transform, schema);
    }

    template <template <typename U> class Reader, typename Writer>
    static
    typename boost::enable_if_c<is_protocol_same<Reader<InputBuffer>, Writer>::value
                             && !protocol_has_multiple_versions<Reader<InputBuffer> >::value, bool>::type
    Apply(const Serializer<Writer>& transform, Reader<InputBuffer>& reader, const Schema& schema, bool base)
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
class Parser<T, Schema, Transform, typename boost::enable_if_c<is_serializing_transform<Transform>::value && !is_same<T, void>::value>::type>
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
inline void Skip(Reader& reader, const T& bonded)
{
    reader.Skip(bonded);
}

template <typename Reader, typename T>
BOND_NO_INLINE void Skip(Reader& reader, const T& bonded, const std::nothrow_t&)
{
    try
    {
        reader.Skip(bonded);
    }
    catch(...)
    {
    }
}


template <typename T, typename Buffer>
inline void Skip(ProtocolReader<Buffer>& /*reader*/, const T& /*bonded*/)
{
    // Not skipping for outer structures
}


template <typename T, typename Buffer>
inline void Skip(ProtocolReader<Buffer>& /*reader*/, const T& /*bonded*/, const std::nothrow_t&)
{
    // Not skipping for outer structures
}


template <typename T, typename Transform, typename Reader, typename Schema>
inline bool Parse(const Transform& transform, Reader& reader, const Schema& schema, const RuntimeSchema* runtime_schema, bool base)
{
    BOOST_VERIFY(!runtime_schema);
    return Parser<T, Schema, Transform>::Apply(transform, reader, schema, base);
}

template <typename T, typename Transform, typename Buffer, typename Schema>
inline bool Parse(const Transform& transform, ProtocolReader<Buffer> reader, const Schema& schema, const RuntimeSchema* runtime_schema, bool base)
{
    BOOST_VERIFY(!base);
    
    if (runtime_schema)
    {
        // Use named variable to avoid gcc silently copying objects (which
        // causes build break, because Parser<> is non-copyable).
        Parser<void, RuntimeSchema, Transform> parser(transform, *runtime_schema);
        return boost::apply_visitor(parser, reader.value);
    }
    else
    {
        // Use named variable to avoid gcc silently copying objects (which
        // causes build break, because Parser<> is non-copyable).
        Parser<T, Schema, Transform> parser(transform, schema);
        return boost::apply_visitor(parser, reader.value);
    }
}


// Visitor which updates in-situ bonded<T> playload by merging it with an object.
template <typename T, typename Buffer>
class InsituMerge
    : public boost::static_visitor<>,
      boost::noncopyable
{
public:
    InsituMerge(const T& var, ProtocolReader<Buffer>& reader)
        : _var(var),
          _reader(reader)
    {}

    
    template <template <typename U> class Reader>
    typename boost::enable_if<is_protocol_enabled<typename remove_const<Reader<Buffer> >::type> >::type
    operator()(Reader<Buffer>& reader) const
    {
        OutputBuffer merged;
        typename Reader<OutputBuffer>::Writer writer(merged);
        
        Merge(_var, reader, writer);

        _reader = Reader<Buffer>(merged.GetBuffer());
    }

    template <typename Reader>
    typename boost::disable_if<is_protocol_enabled<typename remove_const<Reader>::type> >::type
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
    ProtocolReader<Buffer>& _reader;
};


template <typename T, typename Buffer>
inline void Merge(const T& var, ProtocolReader<Buffer>& reader)
{
    boost::apply_visitor(InsituMerge<T, Buffer>(var, reader), reader.value);
}


} // namespace detail

} // namespace bond
