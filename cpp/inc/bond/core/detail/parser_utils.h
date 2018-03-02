// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/reflection.h>


namespace bond
{
namespace detail
{
    template <typename T, typename Transform>
    typename boost::enable_if<is_fast_path_field<T, Transform>, bool>::type
    inline OmittedField(const T& field, const Transform& transform)
    {
        return transform.OmittedField(field);
    }


    template <typename T, typename Transform>
    typename boost::disable_if<is_fast_path_field<T, Transform>, bool>::type
    inline OmittedField(const T&, const Transform& transform)
    {
        return transform.OmittedField(T::id, T::metadata, get_type_id<typename T::field_type>::value);
    }


    template <typename T, typename Transform, typename X>
    typename boost::enable_if<is_fast_path_field<T, Transform>, bool>::type
    inline Field(const T& field, const Transform& transform, const X& value)
    {
        return transform.Field(field, value);
    }


    template <typename T, typename Transform, typename X>
    typename boost::disable_if<is_fast_path_field<T, Transform>, bool>::type
    inline Field(const T&, const Transform& transform, const X& value)
    {
        return transform.Field(T::id, T::metadata, value);
    }

} // namespace detail
} // namespace bond
