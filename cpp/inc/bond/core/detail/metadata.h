// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "sdl.h"
#include "tags.h"

#include <bond/core/bond_types.h>

namespace bond
{


// Variant operator==

/// @brief Compares \a variant for equality against the provided
/// signed integer or enum \a value.
template <typename SignedT>
inline
typename boost::enable_if<is_signed_int_or_enum<SignedT>, bool>::type
operator==(const Variant& variant, SignedT value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == static_cast<SignedT>(variant.int_value);
}


/// @brief Compares \a variant for equality against the provided
/// unsigned integer \a value.
template <typename UnsignedT>
inline
typename boost::enable_if<std::is_unsigned<UnsignedT>, bool>::type
operator==(const Variant& variant, UnsignedT value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == static_cast<UnsignedT>(variant.uint_value);
}


/// @brief Compares \a variant for equality against the provided
/// boolean \a value.
inline bool
operator==(const Variant& variant, bool value)
{
    BOOST_ASSERT(!variant.nothing);
    BOOST_STATIC_ASSERT((std::is_unsigned<bool>::value));

    return value == !!variant.uint_value;
}


/// @brief Compares \a variant for equality against the provided
/// floating point \a value.
inline bool
operator==(const Variant& variant, double value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == variant.double_value;
}


/// @brief Compares \a variant for equality against the provided
/// string \a value.
inline bool
operator==(const Variant& variant, const char* value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == variant.string_value;
}


/// @brief Compares \a variant for equality against the provided
/// string \a value.
inline bool
operator==(const Variant& variant, const std::string& value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == variant.string_value;
}


/// @brief Compares \a variant for equality against the provided
/// wide string \a value.
inline bool
operator==(const Variant& variant, const wchar_t* value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == variant.wstring_value;
}


/// @brief Compares \a variant for equality against the provided
/// wide string \a value.
inline bool
operator==(const Variant& variant, const std::wstring& value)
{
    BOOST_ASSERT(!variant.nothing);
    return value == variant.wstring_value;
}


namespace detail
{


// VariantSet
template <typename T>
inline
typename boost::enable_if<std::is_unsigned<T> >::type
VariantSet(bond::Variant& variant, const T& value)
{
    variant.uint_value = value;
}


template <typename T>
inline
typename boost::enable_if<is_signed_int_or_enum<T> >::type
VariantSet(bond::Variant& variant, const T& value)
{
    variant.int_value = static_cast<int64_t>(value);
}


inline void
VariantSet(bond::Variant& variant, const char* value)
{
    variant.string_value = value;
}


inline void
VariantSet(bond::Variant& variant, const wchar_t* value)
{
    variant.wstring_value = value;
}

inline void
VariantSet(bond::Variant& variant, const double& value)
{
    variant.double_value = value;
}


// VariantGet
inline
void
VariantGet(const bond::Variant& variant, bool& var)
{
    BOOST_ASSERT(!variant.nothing);
    var = !!variant.uint_value;
}


template <typename T>
inline
typename boost::enable_if<std::is_unsigned<T> >::type
VariantGet(const bond::Variant& variant, T& var)
{
    BOOST_ASSERT(!variant.nothing);
    var = static_cast<T>(variant.uint_value);
}


template <typename T>
inline
typename boost::enable_if<is_signed_int_or_enum<T> >::type
VariantGet(const bond::Variant& variant, T& var)
{
    BOOST_ASSERT(!variant.nothing);
    var = static_cast<T>(variant.int_value);
}


template <typename T>
inline
typename boost::enable_if<std::is_floating_point<T> >::type
VariantGet(const bond::Variant& variant, T& var)
{
    BOOST_ASSERT(!variant.nothing);
    var = static_cast<T>(variant.double_value);
}


template <typename T>
inline
typename boost::enable_if<is_string<T> >::type
VariantGet(const bond::Variant& variant, T& var)
{
    BOOST_ASSERT(!variant.nothing);

    const size_t size = variant.string_value.size();
    resize_string(var, static_cast<uint32_t>(size));

    std::copy(
        variant.string_value.begin(),
        variant.string_value.end(),
        detail::make_checked_array_iterator(string_data(var), size));
}


template <typename T>
inline
typename boost::enable_if<is_wstring<T> >::type
VariantGet(const bond::Variant& variant, T& var)
{
    BOOST_ASSERT(!variant.nothing);

    const size_t size = variant.wstring_value.size();
    resize_string(var, static_cast<uint32_t>(size));

    std::copy(
        variant.wstring_value.begin(),
        variant.wstring_value.end(),
        detail::make_checked_array_iterator(string_data(var), size));
}


// Returns name for basic types, overloaded by generated code for enums
template <typename T>
inline
typename boost::disable_if<std::is_enum<T>, const char*>::type
GetTypeName(T, const qualified_name_tag&)
{
    switch (get_type_id<T>::value)
    {
        case BT_BOOL:   return "bool";
        case BT_UINT8:  return "uint8";
        case BT_UINT16: return "uint16";
        case BT_UINT32: return "uint32";
        case BT_UINT64: return "uint64";
        case BT_FLOAT:  return "float";
        case BT_DOUBLE: return "double";
        case BT_INT8:   return "int8";
        case BT_INT16:  return "int16";
        case BT_INT32:  return "int32";
        case BT_INT64:  return "int64";
        default:        BOOST_ASSERT(false);
                        return "unknown_type";
    }
}


template <typename T>
inline
typename boost::enable_if<std::is_enum<T>, const char*>::type
GetTypeName(T e, const qualified_name_tag&)
{
    // In the older versions of generated code we didn't have the overload of
    // GetTypeName for qualified names; delegating to the old GetTypeName.
    return GetTypeName(e);
}


// basic types and enums
template <typename T, typename Enable = void>
struct type
{
    static std::string name()
    {
        return GetTypeName(T(), qualified_name);
    }
};

// service
template <typename T>
struct type<T, typename boost::enable_if<std::is_class<typename T::Schema::methods> >::type>
{
    static std::string name()
    {
        return T::Schema().metadata.qualified_name;
    }
};

// string
template <typename T>
struct type<T, typename boost::enable_if<is_string<T> >::type>
{
    static std::string name()
    {
        return "string";
    }
};


// wstring
template <typename T>
struct type<T, typename boost::enable_if<is_wstring<T> >::type>
{
    static std::string name()
    {
        return "wstring";
    }
};


// blob
template <>
struct type<blob, void>
{
    static std::string name()
    {
        return "blob";
    }
};


// list
template <typename T>
struct type<T, typename boost::enable_if<is_list_container<T> >::type>
{
    static std::string name()
    {
        return "list<" + type<typename element_type<T>::type>::name() + ">";
    }
};


// set
template <typename T>
struct type<T, typename boost::enable_if<is_set_container<T> >::type>
{
    static std::string name()
    {
        return "set<" + type<typename element_type<T>::type>::name() + ">";
    }
};


// map
template <typename T>
struct type<T, typename boost::enable_if<is_map_container<T> >::type>
{
    static std::string name()
    {
        return "map<" + type<typename element_type<T>::type::first_type>::name() + ", "
                      + type<typename element_type<T>::type::second_type>::name() + ">";
    }
};


// bonded
template <typename T>
struct type<bonded<T> >
{
    static std::string name()
    {
        return "bonded<" + type<T>::name() + ">";
    }
};


// maybe
template <typename T>
struct type<maybe<T> >
{
    static std::string name()
    {
        return type<T>::name();
    }
};


// struct
template <typename T>
struct type<T, typename boost::enable_if<has_schema<T> >::type>
{
    static std::string name()
    {
        // We can't assume that the dependent type's Schema::metadata static
        // member is initialized, so instead we call GetMetadata() method.
        return schema<T>::type::GetMetadata().qualified_name;
    }
};


// TypeListBuilder
class TypeListBuilder
{
public:
    TypeListBuilder(std::string& name)
        : _name(name)
    {}

    template <typename T>
    void operator()(const T*)
    {
        if (!_name.empty())
            _name += ", ";

        _name += type<T>::name();
    }

private:
    TypeListBuilder& operator=(const TypeListBuilder&);

    std::string& _name;
};


} // namespace detail


} // namespace bond
