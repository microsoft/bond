// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "bonded.h"
#include "bonded_void.h"
#include <bond/core/bond_reflection.h>
#include "parser.h"
#include "exception.h"
#include "detail/double_pass.h"

namespace bond
{
namespace detail
{

/// @brief Apply transform to serialized struct wrapped in bonded<T>
template <typename Transform, typename T, typename Reader>
typename boost::disable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const bonded<T, Reader>& bonded)
{
    return bonded._Apply(transform);
}


/// @brief Apply transform to serialized container wrapped in value<T, Reader>
template <typename Transform, typename T, typename Reader>
bool inline
ApplyTransform(const Transform& transform, const value<T, Reader>& value)
{
    value._Apply(transform);
    return true;
}


/// @brief Apply transform to an instance of a struct
template <typename Transform, typename T>
typename boost::disable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const T& value)
{
    return StaticParser<const T&>(value).Apply(transform, typename schema<T>::type());
}


/// @brief Apply transform which can modify an instance of a struct
template <typename Transform, typename T>
typename boost::enable_if<is_modifying_transform<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, T& value)
{
    return StaticParser<T&>(value).Apply(transform, typename schema<T>::type());
}


// Specializations for transform requiring double-pass
template <typename Transform, typename T, typename Reader>
typename boost::enable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const bonded<T, Reader>& bonded)
{
    if (transform.NeedPass0())
        return DoublePassApply(transform, bonded);
    else
        return bonded._Apply(transform);
}


template <typename Transform, typename T>
typename boost::enable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const T& value)
{
    if (transform.NeedPass0())
        return DoublePassApply(transform, value);
    else
        return StaticParser<const T&>(value).Apply(transform, typename schema<T>::type());
}

} // namespace detail


template <typename Transform, typename T>
typename boost::enable_if<is_modifying_transform<Transform>, bool>::type
Apply(const Transform& transform, T& value)
{
    return detail::ApplyTransform(transform, value);
}

template <typename Transform, typename T>
typename boost::disable_if<is_modifying_transform<Transform>, bool>::type
Apply(const Transform& transform, const T& value)
{
    return detail::ApplyTransform(transform, value);
}

} // namespace bond

