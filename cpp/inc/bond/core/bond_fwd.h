// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/tags.h"

#include <boost/utility/enable_if.hpp>

#include <stdint.h>

namespace bond
{

class blob;
class InputBuffer;
class RuntimeSchema;

class ProtocolReader;

template <typename T, typename Reader = ProtocolReader>
class bonded;

template <typename Reader>
class bonded<void, Reader>;

template <typename T> struct
remove_bonded;

template <typename T, typename Reader> struct
remove_bonded<bonded<T, Reader> >;

template <typename T, typename Reader, typename Enable = void>
class value;

template <typename Reader>
class StaticParser;

template <typename Reader>
class DynamicParser;

template <typename Reader>
class DOMParser;

template <typename T>
class RequiredFieldValiadator;

struct BuiltInProtocols;

template <typename T, typename Protocols = BuiltInProtocols, typename Validator = RequiredFieldValiadator<T> >
class To;

template <typename T, typename Enable = void> struct
schema_for_passthrough;

template<typename T, typename Enable = void> struct
get_type_id;

template <typename T> struct
may_omit_fields;

template <typename Input>
struct base_input;

struct Metadata;

struct qualified_name_tag;

template <typename Protocols = BuiltInProtocols, typename Transform, typename T, typename boost::enable_if<is_modifying_transform<Transform> >::type* = nullptr>
bool Apply(const Transform& transform, T& value);

template <typename Protocols = BuiltInProtocols, typename Transform, typename T>
bool Apply(const Transform& transform, const T& value);

template <typename Protocols = BuiltInProtocols, typename T, typename Writer>
inline void Marshal(const T& obj, Writer& output);

template <typename Writer, typename Protocols = BuiltInProtocols>
class Marshaler;

template <typename Protocols = BuiltInProtocols, typename Writer>
Marshaler<Writer, Protocols> MarshalTo(Writer& output);

template <typename Writer, typename Protocols = BuiltInProtocols>
class Serializer;

template <typename Protocols = BuiltInProtocols, typename Writer>
Serializer<Writer, Protocols> SerializeTo(Writer& output);

} // bond
