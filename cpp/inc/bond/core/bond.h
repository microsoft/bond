// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/** @file */
#pragma once

#include <bond/core/config.h>

#include "apply.h"
#include "select_protocol.h"

/// namespace bond
namespace bond
{

/// @brief Serialize an object using a protocol writer
///
template <typename Protocols = BuiltInProtocols, typename T, typename Writer>
inline void Serialize(const T& obj, Writer& output)
{
    Apply<Protocols>(Serializer<Writer, Protocols>(output), obj);
}


/// @brief Deserialize an object from a protocol reader
template <typename Protocols = BuiltInProtocols, typename Reader, typename T>
inline void Deserialize(Reader input, T& obj)
{
    Apply<Protocols>(To<T, Protocols>(obj), bonded<T, Reader&>(input));
}


/// @brief Deserialize an object of type T from a protocol reader
template <typename T, typename Protocols = BuiltInProtocols, typename Reader>
inline T Deserialize(Reader input)
{
    T tmp;
    Apply<Protocols>(To<T, Protocols>(tmp), bonded<T, Reader&>(input));
    return tmp;
}


/// @brief Deserialize an object from a protocol reader using runtime schema
template <typename Protocols = BuiltInProtocols, typename Reader, typename T>
inline void Deserialize(Reader input, T& obj, const RuntimeSchema& schema)
{
    Apply<Protocols>(To<T, Protocols>(obj), bonded<void, Reader&>(input, schema));
}


/// @brief Deserialize an object of type T from a protocol reader using runtime schema
template <typename T, typename Protocols = BuiltInProtocols, typename Reader>
inline T Deserialize(Reader input, const RuntimeSchema& schema)
{
    T tmp;
    Apply<Protocols>(To<T, Protocols>(tmp), bonded<void, Reader&>(input, schema));
    return tmp;
}


/// @brief Marshal an object using a protocol writer
template <typename Protocols, typename T, typename Writer>
inline void Marshal(const T& obj, Writer& output)
{
    Apply<Protocols>(Marshaler<Writer, Protocols>(output), obj);
}


/// @brief Unmarshal an object from data stream
template <typename Protocols = BuiltInProtocols, typename Buffer, typename T>
inline void Unmarshal(Buffer input, T& obj)
{
    SelectProtocolAndApply<T, Protocols>(input, To<T, Protocols>(obj));
}


/// @brief Unmarshal an object of type T from data stream
template <typename T, typename Protocols = BuiltInProtocols, typename Buffer>
inline T Unmarshal(Buffer input)
{
    T tmp;
    SelectProtocolAndApply<T, Protocols>(input, To<T, Protocols>(tmp));
    return tmp;
}


/// @brief Initialize a bonded<T> from data stream contained marshaled object
template <typename Protocols = BuiltInProtocols, typename Buffer, typename T>
inline void Unmarshal(Buffer input, bonded<T>& obj)
{
    SelectProtocolAndApply<T, Protocols>(input, boost::ref(obj));
}


/// @brief Unmarshal an object from data stream using a runtime schema
template <typename Protocols = BuiltInProtocols, typename Buffer, typename T>
inline void Unmarshal(Buffer input, T& obj, const RuntimeSchema& schema)
{
    SelectProtocolAndApply<Protocols>(schema, input, To<T, Protocols>(obj));
}


/// @brief Unmarshal an object of type T from data stream using a runtime schema
template <typename T, typename Protocols = BuiltInProtocols, typename Buffer>
inline T Unmarshal(Buffer input, const RuntimeSchema& schema)
{
    T tmp;
    SelectProtocolAndApply<Protocols>(schema, input, To<T, Protocols>(tmp));
    return tmp;
}


/// @brief Initialize a bonded<T> from data stream contained marshaled object
/// using a runtime schema
template <typename Protocols = BuiltInProtocols, typename Buffer, typename T>
inline void Unmarshal(Buffer input, bonded<T>& obj, const RuntimeSchema& schema)
{
    SelectProtocolAndApply<Protocols>(schema, input, boost::ref(obj));
}


/// @brief Merge an object with serialize data and write the result using
/// a protocol writer
template <typename Protocols = BuiltInProtocols, typename T, typename Reader, typename Writer>
inline void Merge(const T& obj, Reader input, Writer& output)
{
    Apply<Protocols>(Merger<T, Writer, Protocols>(obj, output), bonded<T>(input));
}

}
