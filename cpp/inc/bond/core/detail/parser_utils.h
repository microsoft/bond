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


    template <typename T, typename Reader>
    typename boost::enable_if_c<is_reader<Reader>::value && is_nested_field<T>::value,
        bonded<typename T::field_type, Reader&> >::type
    inline GetFieldValue(Reader& input)
    {
        return bonded<typename T::field_type, Reader&>(input);
    }


    template <typename T, typename Reader>
    typename boost::enable_if_c<is_reader<Reader>::value && !is_nested_field<T>::value,
        value<typename T::field_type, Reader&> >::type
    inline GetFieldValue(Reader& input)
    {
        return value<typename T::field_type, Reader&>(input);
    }


    template <typename T, typename X>
    typename boost::disable_if<is_reader<X>,
        typename std::conditional<std::is_const<X>::value,
            const typename T::value_type&,
            typename T::value_type&>::type>::type
    inline GetFieldValue(X& value)
    {
        return T::GetVariable(value);
    }


    template <typename T, typename Transform, typename X>
    typename boost::enable_if_c<is_reader<X>::value && is_fast_path_field<T, Transform>::value, bool>::type
    inline NonBasicTypeField(const T& field, const Transform& transform, X&& value)
    {
        return transform.Field(field, GetFieldValue<T>(std::forward<X>(value)));
    }


    template <typename T, typename Transform, typename X>
    typename boost::disable_if_c<is_reader<X>::value && is_fast_path_field<T, Transform>::value, bool>::type
    inline NonBasicTypeField(const T&, const Transform& transform, X&& value)
    {
        return transform.Field(T::id, T::metadata, GetFieldValue<T>(std::forward<X>(value)));
    }


    template <typename Reader, typename Transform>
    inline bool NonBasicTypeField(const FieldDef& field, const RuntimeSchema& schema, const Transform& transform, Reader& input)
    {
        if (field.type.id == BT_STRUCT)
        {
            return transform.Field(field.id, field.metadata, bonded<void, Reader&>(input, RuntimeSchema(schema, field)));
        }
        else
        {
            BOOST_ASSERT(field.type.id == BT_LIST || field.type.id == BT_SET || field.type.id == BT_MAP);
            return transform.Field(field.id, field.metadata, value<void, Reader&>(input, RuntimeSchema(schema, field)));
        }
    }

} // namespace detail
} // namespace bond
