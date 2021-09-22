// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bonded.h"
#include "bonded_void.h"
#include "detail/double_pass.h"
#include "exception.h"
#include "parser.h"

#include <bond/core/bond_reflection.h>

namespace bond
{
namespace detail
{

/// @brief Apply transform to serialized struct wrapped in bonded<T>
template <typename Protocols, typename Transform, typename T, typename Reader>
typename boost::disable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const bonded<T, Reader>& bonded)
{
    return bonded.template _Apply<Protocols>(transform);
}


/// @brief Apply transform to serialized container wrapped in value<T, Reader>
template <typename Protocols, typename Transform, typename T, typename Reader>
bool inline
ApplyTransform(const Transform& transform, const value<T, Reader>& value)
{
    value.template _Apply<Protocols>(transform);
    return true;
}


/// @brief Apply transform to an instance of a struct
template <typename Protocols, typename Transform, typename T>
typename boost::disable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const T& value)
{
    return StaticParser<const T&>(value).Apply(transform, typename schema<T>::type());
}


/// @brief Apply transform which can modify an instance of a struct
template <typename Protocols, typename Transform, typename T>
typename boost::enable_if<is_modifying_transform<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, T& value)
{
    return StaticParser<T&>(value).Apply(transform, typename schema<T>::type());
}


// Specializations for transform requiring double-pass
template <typename Protocols, typename Transform, typename T, typename Reader>
typename boost::enable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const bonded<T, Reader>& bonded)
{
    if (transform.NeedPass0())
        return DoublePassApply<Protocols>(transform, bonded);
    else
        return bonded.template _Apply<Protocols>(transform);
}


template <typename Protocols, typename Transform, typename T>
typename boost::enable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const T& value)
{
    if (transform.NeedPass0())
        return DoublePassApply<Protocols>(transform, value);
    else
        return StaticParser<const T&>(value).Apply(transform, typename schema<T>::type());
}

} // namespace detail


template <typename Protocols, typename Transform, typename T, typename boost::enable_if<is_modifying_transform<Transform> >::type*>
bool Apply(const Transform& transform, T& value)
{
    return detail::ApplyTransform<Protocols>(transform, value);
}

template <typename Protocols, typename Transform, typename T>
bool Apply(const Transform& transform, const T& value)
{
    return detail::ApplyTransform<Protocols>(transform, value);
}


/// @brief Apply transform to compile-time schema
template <typename T, typename Transform>
bool Apply(const Transform& transform)
{
    SchemaReader input;
    return SchemaReader::Parser{ input }.Apply(transform, typename schema<T>::type{});
}


/// @brief Apply transform to runtime schema
template <typename Transform>
bool Apply(const Transform& transform, const RuntimeSchema& schema)
{
    SchemaReader input;
    return SchemaReader::Parser{ input }.Apply(transform, schema);
}

} // namespace bond
