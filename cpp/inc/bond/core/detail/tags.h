// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "nonassignable.h"

#include <bond/core/bond_const_enum.h>

#include <type_traits>

namespace bond
{

struct Metadata;

// Used to get qualified name of enum using generated GetTypeName
struct qualified_name_tag
{};


BOND_STATIC_CONSTEXPR qualified_name_tag qualified_name = {};


// A serializing transform accepts fields of a structure
struct serializing_transform_tag
{};


// A deserializing transform accepts fields from serialized payload
struct deserializing_transform_tag
{};


// A modifying transform gets non-const access to struct fields
struct modifying_transform_tag
{};


struct Transform
    : detail::nonassignable
{
    bool OmittedField(uint16_t /*id*/, const Metadata& /*metadata*/, BondDataType /*type*/) const
    {
        return false;
    }

    template <typename Field>
    bool OmittedField(const Field&) const
    {
        return false;
    }
};


/// @brief Base class for serializing transforms
struct SerializingTransform
    : Transform
{
    typedef serializing_transform_tag transform_category;
};


/// @brief Base class for deserializing transforms
struct DeserializingTransform
    : Transform
{
    typedef deserializing_transform_tag transform_category;
};


/// @brief Base class for transforms which modify a Bond type instance
struct ModifyingTransform
    : Transform
{
    typedef modifying_transform_tag transform_category;
};


template <typename T, typename Unused = void> struct
is_serializing_transform
    : std::is_base_of<SerializingTransform, T> {};


template <typename T, typename Unused = void> struct
is_deserializing_transform
    : std::is_base_of<DeserializingTransform, T> {};


template <typename T, typename Unused = void> struct
is_modifying_transform
    : std::is_base_of<ModifyingTransform, T> {};


} // namespace bond
