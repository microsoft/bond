// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

namespace bond
{

namespace detail 
{


// Sanity check to assert that manually expended switch statement are consistent
// with the canonical definition of type promotion in is_matching metafunction.

template <typename T>
bool IsMatching(BondDataType type)
{
    switch (type)
    {
        case bond::BT_BOOL:
            return is_matching<bool, T>::value;

        case bond::BT_UINT8:
            return is_matching<uint8_t, T>::value;

        case bond::BT_UINT16:
            return is_matching<uint16_t, T>::value;

        case bond::BT_UINT32:
            return is_matching<uint32_t, T>::value;

        case bond::BT_UINT64:
            return is_matching<uint64_t, T>::value;

        case bond::BT_FLOAT:
            return is_matching<float, T>::value;

        case bond::BT_DOUBLE:
            return is_matching<double, T>::value;

        case bond::BT_STRING:
            return is_matching<std::string, T>::value;
            
        case bond::BT_WSTRING:
            return is_matching<std::wstring, T>::value;

        case bond::BT_INT8:
            return is_matching<int8_t, T>::value;

        case bond::BT_INT16:
            return is_matching<int16_t, T>::value;

        case bond::BT_INT32:
            return is_matching<int32_t, T>::value;

        case bond::BT_INT64:
            return is_matching<int64_t, T>::value;

        default:
            BOOST_ASSERT(false);
            return false;
    }
}


template <typename Transform, typename Reader>
inline bool BasicTypeField(uint16_t id, const Metadata& metadata, BondDataType type, const Transform& transform, Reader& input)
{
    switch (type)
    {
        case bond::BT_BOOL:
            return transform.Field(id, metadata, value<bool, Reader&>(input));

        case bond::BT_UINT8:
            return transform.Field(id, metadata, value<uint8_t, Reader&>(input));

        case bond::BT_UINT16:
            return transform.Field(id, metadata, value<uint16_t, Reader&>(input));

        case bond::BT_UINT32:
            return transform.Field(id, metadata, value<uint32_t, Reader&>(input));

        case bond::BT_UINT64:
            return transform.Field(id, metadata, value<uint64_t, Reader&>(input));
            
        case bond::BT_FLOAT:
            return transform.Field(id, metadata, value<float, Reader&>(input));

        case bond::BT_DOUBLE:
            return transform.Field(id, metadata, value<double, Reader&>(input));

        case bond::BT_STRING:
            return transform.Field(id, metadata, value<std::string, Reader&>(input));

        case bond::BT_WSTRING:
            return transform.Field(id, metadata, value<std::wstring, Reader&>(input));

        case bond::BT_INT8:
            return transform.Field(id, metadata, value<int8_t, Reader&>(input));

        case bond::BT_INT16:
            return transform.Field(id, metadata, value<int16_t, Reader&>(input));

        case bond::BT_INT32:
            return transform.Field(id, metadata, value<int32_t, Reader&>(input));

        case bond::BT_INT64:
            return transform.Field(id, metadata, value<int64_t, Reader&>(input));

        default:
            BOOST_ASSERT(false);
            return false;
    }
}


template <typename T, typename Reader>
inline void BasicTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    BOOST_STATIC_ASSERT(!is_container<T>::value);

    switch (type)
    {
        case bond::BT_BOOL:
            return DeserializeElements(var, value<bool, Reader&>(input, false), size);

        case bond::BT_UINT8:
            return DeserializeElements(var, value<uint8_t, Reader&>(input, false), size);

        case bond::BT_UINT16:
            return DeserializeElements(var, value<uint16_t, Reader&>(input, false), size);

        case bond::BT_UINT32:
            return DeserializeElements(var, value<uint32_t, Reader&>(input, false), size);

        case bond::BT_UINT64:
            return DeserializeElements(var, value<uint64_t, Reader&>(input, false), size);

        case bond::BT_FLOAT:
            return DeserializeElements(var, value<float, Reader&>(input, false), size);

        case bond::BT_DOUBLE:
            return DeserializeElements(var, value<double, Reader&>(input, false), size);

        case bond::BT_STRING:
            return DeserializeElements(var, value<std::string, Reader&>(input, false), size);
            
        case bond::BT_WSTRING:
            return DeserializeElements(var, value<std::wstring, Reader&>(input, false), size);

        case bond::BT_INT8:
            return DeserializeElements(var, value<int8_t, Reader&>(input, false), size);

        case bond::BT_INT16:
            return DeserializeElements(var, value<int16_t, Reader&>(input, false), size);

        case bond::BT_INT32:
            return DeserializeElements(var, value<int32_t, Reader&>(input, false), size);

        case bond::BT_INT64:
            return DeserializeElements(var, value<int64_t, Reader&>(input, false), size);

        default:
            BOOST_ASSERT(false);
            return;
    }
}


// MatchingTypeContainer function are manually expended versions of BasicTypeContainer
// using the type information about destination container. This helps with compilation speed.
template <typename T, typename Reader>
typename boost::enable_if<is_type_alias<typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    if (type == get_type_id<typename element_type<T>::type>::value)
    {
        DeserializeElements(var, value<typename element_type<T>::type, Reader&>(input, false), size);
    }
    else
    {
        BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

        while (size--)
            input.Skip(type);
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_same<bool, typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    switch (type)
    {
        case bond::BT_BOOL:
            return DeserializeElements(var, value<bool, Reader&>(input, false), size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

            while (size--)
                input.Skip(type);
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_string<typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    switch (type)
    {
        case bond::BT_STRING:
            return DeserializeElements(var, value<std::string, Reader&>(input, false), size);
            
        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

            while (size--)
                input.Skip(type);
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_wstring<typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    switch (type)
    {
        case bond::BT_WSTRING:
            return DeserializeElements(var, value<std::wstring, Reader&>(input, false), size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

            while (size--)
                input.Skip(type);
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_floating_point<typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    switch (type)
    {
        case bond::BT_FLOAT:
            return DeserializeElements(var, value<float, Reader&>(input, false), size);

        case bond::BT_DOUBLE:
            return DeserializeElements(var, value<double, Reader&>(input, false), size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

            while (size--)
                input.Skip(type);
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_matching<uint8_t, typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    switch (type)
    {
        case bond::BT_UINT8:
            return DeserializeElements(var, value<uint8_t, Reader&>(input, false), size);

        case bond::BT_UINT16:
            return DeserializeElements(var, value<uint16_t, Reader&>(input, false), size);

        case bond::BT_UINT32:
            return DeserializeElements(var, value<uint32_t, Reader&>(input, false), size);

        case bond::BT_UINT64:
            return DeserializeElements(var, value<uint64_t, Reader&>(input, false), size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

            while (size--)
                input.Skip(type);
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_matching<int8_t, typename element_type<T>::type> >::type
inline MatchingTypeContainer(T& var, BondDataType type, Reader& input, uint32_t size)
{
    switch (type)
    {
        case bond::BT_INT8:
            return DeserializeElements(var, value<int8_t, Reader&>(input, false), size);

        case bond::BT_INT16:
            return DeserializeElements(var, value<int16_t, Reader&>(input, false), size);

        case bond::BT_INT32:
            return DeserializeElements(var, value<int32_t, Reader&>(input, false), size);

        case bond::BT_INT64:
            return DeserializeElements(var, value<int64_t, Reader&>(input, false), size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type>(type));

            while (size--)
                input.Skip(type);
            break;
    }
}

// MatchingMapByKey function are manually expended versions of MapByKey using the type 
// information about destination container. This helps with compilation speed.

template <typename T, typename E, typename Reader>
typename boost::enable_if<is_type_alias<typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    if (keyType == get_type_id<typename element_type<T>::type::first_type>::value)
    {
        return DeserializeMapElements(var, value<typename element_type<T>::type::first_type, Reader&>(input, false), element, size);
    }
    else
    {
        BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

        while (size--)
        {
            input.Skip(keyType);
            element.Skip();
        }
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_same<bool, typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_BOOL:
            return DeserializeMapElements(var, value<bool, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

            while (size--)
            {
                input.Skip(keyType);
                element.Skip();
            }
            break;
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_string<typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_STRING:
            return DeserializeMapElements(var, value<std::string, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

            while (size--)
            {
                input.Skip(keyType);
                element.Skip();
            }
            break;
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_wstring<typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_WSTRING:
            return DeserializeMapElements(var, value<std::wstring, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

            while (size--)
            {
                input.Skip(keyType);
                element.Skip();
            }
            break;
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_floating_point<typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_FLOAT:
            return DeserializeMapElements(var, value<float, Reader&>(input, false), element, size);

        case bond::BT_DOUBLE:
            return DeserializeMapElements(var, value<double, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

            while (size--)
            {
                input.Skip(keyType);
                element.Skip();
            }
            break;
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_matching<uint8_t, typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_UINT8:
            return DeserializeMapElements(var, value<uint8_t, Reader&>(input, false), element, size);

        case bond::BT_UINT16:
            return DeserializeMapElements(var, value<uint16_t, Reader&>(input, false), element, size);

        case bond::BT_UINT32:
            return DeserializeMapElements(var, value<uint32_t, Reader&>(input, false), element, size);

        case bond::BT_UINT64:
            return DeserializeMapElements(var, value<uint64_t, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

            while (size--)
            {
                input.Skip(keyType);
                element.Skip();
            }
            break;
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_matching<int8_t, typename element_type<T>::type::first_type> >::type
inline MatchingMapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_INT8:
            return DeserializeMapElements(var, value<int8_t, Reader&>(input, false), element, size);

        case bond::BT_INT16:
            return DeserializeMapElements(var, value<int16_t, Reader&>(input, false), element, size);

        case bond::BT_INT32:
            return DeserializeMapElements(var, value<int32_t, Reader&>(input, false), element, size);

        case bond::BT_INT64:
            return DeserializeMapElements(var, value<int64_t, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::first_type>(keyType));

            while (size--)
            {
                input.Skip(keyType);
                element.Skip();
            }
            break;
    }
}



template <typename T, typename E, typename Reader>
typename boost::disable_if<is_map_container<T> >::type  
inline MapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    switch (keyType)
    {
        case bond::BT_BOOL:
            return DeserializeMapElements(var, value<bool, Reader&>(input, false), element, size);

        case bond::BT_UINT8:
            return DeserializeMapElements(var, value<uint8_t, Reader&>(input, false), element, size);

        case bond::BT_UINT16:
            return DeserializeMapElements(var, value<uint16_t, Reader&>(input, false), element, size);

        case bond::BT_UINT32:
            return DeserializeMapElements(var, value<uint32_t, Reader&>(input, false), element, size);

        case bond::BT_UINT64:
            return DeserializeMapElements(var, value<uint64_t, Reader&>(input, false), element, size);

        case bond::BT_FLOAT:
            return DeserializeMapElements(var, value<float, Reader&>(input, false), element, size);

        case bond::BT_DOUBLE:
            return DeserializeMapElements(var, value<double, Reader&>(input, false), element, size);

        case bond::BT_STRING:
            return DeserializeMapElements(var, value<std::string, Reader&>(input, false), element, size);
            
        case bond::BT_WSTRING:
            return DeserializeMapElements(var, value<std::wstring, Reader&>(input, false), element, size);

        case bond::BT_INT8:
            return DeserializeMapElements(var, value<int8_t, Reader&>(input, false), element, size);

        case bond::BT_INT16:
            return DeserializeMapElements(var, value<int16_t, Reader&>(input, false), element, size);

        case bond::BT_INT32:
            return DeserializeMapElements(var, value<int32_t, Reader&>(input, false), element, size);

        case bond::BT_INT64:
            return DeserializeMapElements(var, value<int64_t, Reader&>(input, false), element, size);

        default:
            BOOST_ASSERT(false);
            return;
    }
}


template <typename T, typename E, typename Reader>
typename boost::enable_if<is_map_element_matching<E, T> >::type  
inline MapByKey(T& var, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    return MatchingMapByKey(var, keyType, element, input, size);
}


template <typename T, typename E, typename Reader>
typename boost::disable_if_c<!is_map_container<T>::value || is_map_element_matching<E, T>::value>::type
inline MapByKey(T&, BondDataType keyType, const E& element, Reader& input, uint32_t size)
{
    while (size--)
    {
        input.Skip(keyType);
        element.Skip();
    }
}


template <typename T, typename Reader>
inline void MapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_BOOL:
            return MapByKey(var, keyType, value<bool, Reader&>(input, false), input, size);

        case bond::BT_UINT8:
            return MapByKey(var, keyType, value<uint8_t, Reader&>(input, false), input, size);

        case bond::BT_UINT16:
            return MapByKey(var, keyType, value<uint16_t, Reader&>(input, false), input, size);

        case bond::BT_UINT32:
            return MapByKey(var, keyType, value<uint32_t, Reader&>(input, false), input, size);

        case bond::BT_UINT64:
            return MapByKey(var, keyType, value<uint64_t, Reader&>(input, false), input, size);

        case bond::BT_FLOAT:
            return MapByKey(var, keyType, value<float, Reader&>(input, false), input, size);

        case bond::BT_DOUBLE:
            return MapByKey(var, keyType, value<double, Reader&>(input, false), input, size);

        case bond::BT_STRING:
            return MapByKey(var, keyType, value<std::string, Reader&>(input, false), input, size);
            
        case bond::BT_WSTRING:
            return MapByKey(var, keyType, value<std::wstring, Reader&>(input, false), input, size);

        case bond::BT_INT8:
            return MapByKey(var, keyType, value<int8_t, Reader&>(input, false), input, size);

        case bond::BT_INT16:
            return MapByKey(var, keyType, value<int16_t, Reader&>(input, false), input, size);

        case bond::BT_INT32:
            return MapByKey(var, keyType, value<int32_t, Reader&>(input, false), input, size);

        case bond::BT_INT64:
            return MapByKey(var, keyType, value<int64_t, Reader&>(input, false), input, size);

        default:
            BOOST_ASSERT(false);
            break; 
    }
}


// MatchingMapByElement function are manually expended versions of MapByElement using 
// the type information about destination container. This helps with compilation speed.

template <typename T, typename Reader>
typename boost::enable_if<is_type_alias<typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    if (elementType == get_type_id<typename element_type<T>::type::second_type>::value)
    {
        MapByKey(var, keyType, value<typename element_type<T>::type::second_type, Reader&>(input, false), input, size);
    }
    else
    {
        BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

        while (size--)
        {
            input.Skip(keyType);
            input.Skip(elementType);
        }
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_same<bool, typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_BOOL:
            return MapByKey(var, keyType, value<bool, Reader&>(input, false), input, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

            while (size--)
            {
                input.Skip(keyType);
                input.Skip(elementType);
            }
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_string<typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_STRING:
            return MapByKey(var, keyType, value<std::string, Reader&>(input, false), input, size);
            
        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

            while (size--)
            {
                input.Skip(keyType);
                input.Skip(elementType);
            }
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_wstring<typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_WSTRING:
            return MapByKey(var, keyType, value<std::wstring, Reader&>(input, false), input, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

            while (size--)
            {
                input.Skip(keyType);
                input.Skip(elementType);
            }
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_floating_point<typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_FLOAT:
            return MapByKey(var, keyType, value<float, Reader&>(input, false), input, size);

        case bond::BT_DOUBLE:
            return MapByKey(var, keyType, value<double, Reader&>(input, false), input, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

            while (size--)
            {
                input.Skip(keyType);
                input.Skip(elementType);
            }
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_matching<uint8_t, typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_UINT8:
            return MapByKey(var, keyType, value<uint8_t, Reader&>(input, false), input, size);

        case bond::BT_UINT16:
            return MapByKey(var, keyType, value<uint16_t, Reader&>(input, false), input, size);

        case bond::BT_UINT32:
            return MapByKey(var, keyType, value<uint32_t, Reader&>(input, false), input, size);

        case bond::BT_UINT64:
            return MapByKey(var, keyType, value<uint64_t, Reader&>(input, false), input, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

            while (size--)
            {
                input.Skip(keyType);
                input.Skip(elementType);
            }
            break;
    }
}


template <typename T, typename Reader>
typename boost::enable_if<is_matching<int8_t, typename element_type<T>::type::second_type> >::type
inline MatchingMapByElement(T& var, BondDataType keyType, BondDataType elementType, Reader& input, uint32_t size)
{
    switch (elementType)
    {
        case bond::BT_INT8:
            return MapByKey(var, keyType, value<int8_t, Reader&>(input, false), input, size);

        case bond::BT_INT16:
            return MapByKey(var, keyType, value<int16_t, Reader&>(input, false), input, size);

        case bond::BT_INT32:
            return MapByKey(var, keyType, value<int32_t, Reader&>(input, false), input, size);

        case bond::BT_INT64:
            return MapByKey(var, keyType, value<int64_t, Reader&>(input, false), input, size);

        default:
            BOOST_ASSERT(!IsMatching<typename element_type<T>::type::second_type>(elementType));

            while (size--)
            {
                input.Skip(keyType);
                input.Skip(elementType);
            }
            break;
    }
}


} // namespace detail

} // namespace bond
