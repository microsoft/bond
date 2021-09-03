// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/blob.h>
#include <bond/core/containers.h>

#include <exception>
#include <stdio.h>

namespace bond
{


template <typename Buffer, typename T, typename Enable = void> struct
implements_varint_write
    : std::false_type {};


template <typename Buffer, typename T> struct
implements_varint_write<Buffer, T,
#ifdef BOND_NO_SFINAE_EXPR
    typename boost::enable_if<check_method<void (Buffer::*)(T), &Buffer::WriteVariableUnsigned> >::type>
#else
    detail::mpl::void_t<decltype(std::declval<Buffer>().WriteVariableUnsigned(std::declval<T>()))>>
#endif
    : std::true_type {};


// GenericWriteVariableUnsigned and WriteVariableUnsigned are mutually
// recursive, so we need a forward declaration of one of them.
template<typename Buffer, typename T>
inline void GenericWriteVariableUnsigned(Buffer& output, T value);


template<typename Buffer, typename T>
inline
typename boost::enable_if<implements_varint_write<Buffer, T> >::type
WriteVariableUnsigned(Buffer& output, T value)
{
    BOOST_STATIC_ASSERT(std::is_unsigned<T>::value);

    // Use Buffer's implementation of WriteVariableUnsigned
    output.WriteVariableUnsigned(value);
}


template<typename Buffer, typename T>
inline
typename boost::disable_if<implements_varint_write<Buffer, T> >::type
WriteVariableUnsigned(Buffer& output, T value)
{
    BOOST_STATIC_ASSERT(std::is_unsigned<T>::value);

    // Use generic WriteVariableUnsigned
    GenericWriteVariableUnsigned(output, value);
}


template<typename Buffer, typename T>
BOND_NO_INLINE
inline void GenericWriteVariableUnsigned(Buffer& output, T value)
{
    T x = value;

    if (value >>= 7)
    {
        output.Write(static_cast<uint8_t>(x | 0x80));
        WriteVariableUnsigned(output, value);
    }
    else
    {
        output.Write(static_cast<uint8_t>(x));
    }
}


template <typename Buffer, typename T, typename Enable = void> struct
implements_varint_read
    : std::false_type {};


template <typename Buffer, typename T> struct
implements_varint_read<Buffer, T,
#ifdef BOND_NO_SFINAE_EXPR
    typename boost::enable_if<check_method<void (Buffer::*)(T&), &Buffer::ReadVariableUnsigned> >::type>
#else
    detail::mpl::void_t<decltype(std::declval<Buffer>().ReadVariableUnsigned(std::declval<T&>()))>>
#endif
    : std::true_type {};


template<typename Buffer, typename T>
inline
typename boost::enable_if<implements_varint_read<Buffer, T> >::type
ReadVariableUnsigned(Buffer& input, T& value)
{
    BOOST_STATIC_ASSERT(std::is_unsigned<T>::value);

    // Use Buffer's implementation of ReadVariableUnsigned
    input.ReadVariableUnsigned(value);
}


template<typename Buffer, typename T>
BOND_NO_INLINE
void GenericReadVariableUnsigned(Buffer& input, T& value)
{
    value = 0;
    uint8_t byte;
    uint32_t shift = 0;

    do
    {
        input.Read(byte);

        T part = byte & 0x7f;
        value += part << shift;
        shift += 7;
    }
    while(byte >= 0x80);
}


template<typename Buffer, typename T>
inline
typename boost::disable_if<implements_varint_read<Buffer, T> >::type
ReadVariableUnsigned(Buffer& input, T& value)
{
    BOOST_STATIC_ASSERT(std::is_unsigned<T>::value);

    // Use generic ReadVariableUnsigned
    GenericReadVariableUnsigned(input, value);
}


// ZigZag encoding
template<typename T>
inline
typename std::make_unsigned<T>::type EncodeZigZag(T value)
{
    return (value << 1) ^ (value >> (sizeof(T) * 8 - 1));
}

// ZigZag decoding
template<typename T>
inline
typename std::make_signed<T>::type DecodeZigZag(T value)
{
    return (value >> 1) ^ (-static_cast<typename std::make_signed<T>::type>((value & 1)));
}


namespace detail
{

// HexDigit
inline char HexDigit(int n)
{
    char d = n & 0xf;
    return d < 10 ? ('0' + d) : ('a' + d - 10);
}

inline int HexDigit(char c)
{
    if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    else if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    else
        return c - '0';
}

template <typename T, typename Enable = void> struct
string_char_int_type;

template <typename T> struct
string_char_int_type<T, typename boost::enable_if<is_string<T> >::type>
{
    typedef uint8_t type;
};

template <typename T> struct
string_char_int_type<T, typename boost::enable_if<is_wstring<T> >::type>
{
    typedef uint16_t type;
};

template <typename Buffer, typename T>
typename boost::enable_if_c<(sizeof(typename element_type<T>::type) == sizeof(typename string_char_int_type<T>::type))>::type
inline ReadStringData(Buffer& input, T& value, uint32_t length)
{
    resize_string(value, length);
    input.Read(string_data(value), length * sizeof(typename element_type<T>::type));
}

template <typename Buffer, typename T>
typename boost::enable_if_c<(sizeof(typename element_type<T>::type) > sizeof(typename string_char_int_type<T>::type))>::type
inline ReadStringData(Buffer& input, T& value, uint32_t length)
{
    resize_string(value, length);
    typename element_type<T>::type* data = string_data(value);
    typename element_type<T>::type* const data_end = data + length;
    typename string_char_int_type<T>::type ch;
    for (; data != data_end; ++data)
    {
        input.Read(ch);
        *data = static_cast<typename element_type<T>::type>(ch);
    }
}

template <typename Buffer, typename T>
typename boost::enable_if_c<(sizeof(typename element_type<T>::type) == sizeof(typename string_char_int_type<T>::type))>::type
inline WriteStringData(Buffer& output, const T& value, uint32_t length)
{
    output.Write(string_data(value), length * sizeof(typename element_type<T>::type));
}

template <typename Buffer, typename T>
typename boost::enable_if_c<(sizeof(typename element_type<T>::type) > sizeof(typename string_char_int_type<T>::type))>::type
inline WriteStringData(Buffer& output, const T& value, uint32_t length)
{
    const typename element_type<T>::type* data = string_data(value);
    const typename element_type<T>::type* const data_end = data + length;
    typename string_char_int_type<T>::type ch;
    for (; data != data_end; ++data)
    {
        ch = static_cast<typename string_char_int_type<T>::type>(*data);
        output.Write(ch);
    }
}

} // namespace detail

} // namespace bond
