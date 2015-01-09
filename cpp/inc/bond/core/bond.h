// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/** @file */
#pragma once

#include "apply.h"
#include "select_protocol.h"

/// namespace bond
namespace bond
{

/// @brief Serialize an object using a protocol writer
///
template <typename T, typename Writer>
inline void Serialize(const T& obj, Writer& output)
{
    Apply(Serializer<Writer>(output), obj);
}


/// @brief Deserialize an object from a protocol reader
template <typename T, typename Reader>
inline void Deserialize(Reader input, T& obj)
{
    Apply(To<T>(obj), bonded<T, Reader&>(input));
}


/// @brief Deserialize an object of type T from a protocol reader
template <typename T, typename Reader>
inline T Deserialize(Reader input)
{
    T tmp;
    Apply(To<T>(tmp), bonded<T, Reader&>(input));
    return tmp;
}


/// @brief Deserialize an object from a protocol reader using runtime schema
template <typename T, typename Reader>
inline void Deserialize(Reader input, T& obj, const RuntimeSchema& schema)
{
    Apply(To<T>(obj), bonded<void, Reader&>(input, schema));
}


/// @brief Deserialize an object of type T from a protocol reader using runtime schema
template <typename T, typename Reader>
inline T Deserialize(Reader input, const RuntimeSchema& schema)
{
    T tmp;
    Apply(To<T>(tmp), bonded<void, Reader&>(input, schema));
    return tmp;
}


/// @brief Marshal an object using a protocol writer
template <typename T, typename Writer>
inline void Marshal(const T& obj, Writer& output)
{
    Apply(Marshaler<Writer>(output), obj);
}


/// @brief Unmarshal an object from data stream
template <typename T, typename Buffer>
inline void Unmarshal(Buffer input, T& obj)
{
    SelectProtocolAndApply<T>(input, To<T>(obj));
}


/// @brief Unmarshal an object of type T from data stream
template <typename T, typename Buffer>
inline T Unmarshal(Buffer input)
{
    T tmp;
    SelectProtocolAndApply<T>(input, To<T>(tmp));
    return tmp;
}


/// @brief Initialize a bonded<T> from data stream contained marshaled object
template <typename T, typename Buffer>
inline void Unmarshal(Buffer input, bonded<T>& obj)
{
    SelectProtocolAndApply<T>(input, boost::ref(obj));
}


/// @brief Unmarshal an object from data stream using a runtime schema
template <typename T, typename Buffer>
inline void Unmarshal(Buffer input, T& obj, const RuntimeSchema& schema)
{
    SelectProtocolAndApply(schema, input, To<T>(obj));
}


/// @brief Unmarshal an object of type T from data stream using a runtime schema
template <typename T, typename Buffer>
inline T Unmarshal(Buffer input, const RuntimeSchema& schema)
{
    T tmp;
    SelectProtocolAndApply(schema, input, To<T>(tmp));
    return tmp;
}


/// @brief Initialize a bonded<T> from data stream contained marshaled object 
/// using a runtime schema
template <typename T, typename Buffer>
inline void Unmarshal(Buffer input, bonded<T>& obj, const RuntimeSchema& schema)
{
    SelectProtocolAndApply(schema, input, boost::ref(obj));
}


/// @brief Merge an object with serialize data and write the result using 
/// a protocol writer
template <typename T, typename Reader, typename Writer>
inline void Merge(const T& obj, Reader input, Writer& output)
{
    Apply(Merger<T, Writer>(obj, output), bonded<T>(input));
}

}
