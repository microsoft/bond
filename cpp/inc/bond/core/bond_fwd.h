// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "config.h"
#include <bond/core/bond_const_enum.h>
#include <stdint.h>

namespace bond
{

class blob;
class InputBuffer;
class RuntimeSchema;

template <typename Buffer>
struct ProtocolReader;

template <typename T, typename Reader = ProtocolReader<InputBuffer> >
class bonded;

template <typename Reader>
class bonded<void, Reader>;

template <typename V> struct 
remove_bonded;

template <typename V> struct 
remove_bonded<bonded<V> >;

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

template <typename T, typename Validator = RequiredFieldValiadator<T> >
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

}
