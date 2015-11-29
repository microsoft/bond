// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#define RAPIDJSON_NO_INT64DEFINE
#define RAPIDJSON_ASSERT BOOST_ASSERT
#define RAPIDJSON_PARSE_ERROR(err, offset) bond::RapidJsonException(rapidjson::GetParseError_En(err), offset)

#include <bond/core/bond_const_enum.h>
#include <bond/core/exception.h>
#include <boost/call_traits.hpp>
#include <boost/noncopyable.hpp>
#include <boost/locale.hpp>
#include "rapidjson/rapidjson.h"
#include "rapidjson/error/en.h"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

namespace bond
{
namespace detail
{


// Adapter from Bond input stream to rapidjson read-only stream
template <typename Buffer>
class RapidJsonInputStream
{
public:
    typedef char Ch;

    RapidJsonInputStream(typename boost::call_traits<Buffer>::reference input)
        : input(&input),
          current(0),
          count(0)
    {
        input.Read(current);
    }

    RapidJsonInputStream(const RapidJsonInputStream& that, typename boost::call_traits<Buffer>::reference input)
        : input(&input),
          current(that.current),
          count(that.count)
    {}

    char Peek()
    {
        if (!current)
            input->Read(current);

        return current;
    }

    size_t Tell() const
    {
        return count;
    }

    char Take()
    {
        char c = current;
        current = '\0';

        if (!c)
            input->Read(c);

        ++count;
        return c;
    }

    // not implemented for read only stream
    char* PutBegin() { BOOST_ASSERT(false); return 0; }
    void Put(char) { BOOST_ASSERT(false); }
    size_t PutEnd(char*) { BOOST_ASSERT(false); return 0; }

    RapidJsonInputStream& operator=(const RapidJsonInputStream& that)
    {
        // rapidjson reader makes a local copy of stream within some functions
        // and assigns it back to its main stream variable before function exit.
        BOOST_ASSERT(input == that.input);
        current = that.current;
        count = that.count;
        return *this;
    }

private:
    Buffer* input;
    uint8_t current;
    size_t count;
};


// Adapter from Bond output stream to rapidjson write-only stream
template <typename Buffer>
class RapidJsonOutputStream
{
public:
    RapidJsonOutputStream(typename boost::call_traits<Buffer>::reference output)
        : output(output)
    {
    }

    // not implemented for write-only stream
    char Peek() { BOOST_ASSERT(false); return 0; }
    size_t Tell() const { BOOST_ASSERT(false); return 0; }
    char Take() { BOOST_ASSERT(false); return 0; }
    size_t PutEnd(char* begin) { BOOST_ASSERT(false); return 0; }
    char* PutBegin() { BOOST_ASSERT(false); return 0; }

    void Put(char c)
    {
        output.Write(c);
    }

private:
    Buffer& output;
};


// Specialization to allow using string as input buffer for simple JSON reader
template <>
struct RapidJsonInputStream<const rapidjson::UTF8<>::Ch*> : rapidjson::StringStream
{
    RapidJsonInputStream(const char* buffer)
        : rapidjson::StringStream(buffer)
    {}

    RapidJsonInputStream(const RapidJsonInputStream& that, const char*)
        : rapidjson::StringStream(that)
    {}
};


class JsonTypeMatching : boost::noncopyable
{
public:
    JsonTypeMatching(BondDataType type, BondDataType schema, bool is_enum)
        : matchesObject(type == BT_STRUCT && type == schema),
          matchesArray((type == BT_MAP || type == BT_LIST || type == BT_SET) && type == schema),
          matchesNull(type == BT_LIST && type == schema),
          matchesInt(type >= BT_INT8 && type <= BT_INT64),
          matchesInt64(type == BT_INT64),
          matchesUint(type >= BT_UINT8 && type <= BT_UINT64),
          matchesUint64(type == BT_UINT64),
          matchesNumber(type >= BT_FLOAT && type <= BT_DOUBLE),
          matchesString(type == BT_STRING || type == BT_WSTRING || is_enum),
          matchesBool(type == BT_BOOL)
    {}


    bool TypeMatch(const rapidjson::Value& value) const
    {
        return ComplexTypeMatch(value) || BasicTypeMatch(value);
    }

    bool ComplexTypeMatch(const rapidjson::Value& value) const
    {
        return ((value.IsObject() && matchesObject)
            || (value.IsArray() && matchesArray)
            || (value.IsNull() && matchesNull));
    }

    bool BasicTypeMatch(const rapidjson::Value& value) const
    {
        return ((value.IsString() && matchesString)
            || (value.IsUint() && matchesUint)
            || (value.IsInt() && matchesInt)
            || (value.IsUint64() && matchesUint64)
            || (value.IsInt64() && matchesInt64)
            || (value.IsNumber() && matchesNumber)
            || (value.IsBool() && matchesBool));
    }

private:
    const bool matchesObject;
    const bool matchesArray;
    const bool matchesNull;
    const bool matchesInt;
    const bool matchesInt64;
    const bool matchesUint;
    const bool matchesUint64;
    const bool matchesNumber;
    const bool matchesString;
    const bool matchesBool;
};


// bool
inline void Read(const rapidjson::Value& value, bool& var)
{
    var = value.GetBool();
}

// enum
template <typename T>
typename boost::enable_if<is_enum<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    if (value.IsString())
        ToEnum(var, value.GetString());
    else
        var = static_cast<T>(value.GetInt());
}

// floating point
template <typename T>
typename boost::enable_if<is_floating_point<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    var = static_cast<T>(value.GetDouble());
}

// signed integer
template <typename T>
typename boost::enable_if<is_signed_int<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    var = static_cast<T>(value.GetInt64());
}

// unsigned integer
template <typename T>
typename boost::enable_if<is_unsigned<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    var = static_cast<T>(value.GetUint64());
}

// strings
template <typename T>
typename boost::enable_if<is_string<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    uint32_t length = value.GetStringLength();

    resize_string(var, length);
    memcpy(string_data(var), value.GetString(), length);
}


// wstring
template <typename T>
typename boost::enable_if<is_wstring<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    std::basic_string<uint16_t> str =
        boost::locale::conv::utf_to_utf<uint16_t>(
            value.GetString(), value.GetString() + value.GetStringLength(), boost::locale::conv::stop);
    const uint32_t length = static_cast<uint32_t>(str.size());

    resize_string(var, length);
    std::copy(str.begin(), str.end(), string_data(var));
}


// type alias
template <typename T>
typename boost::enable_if<is_type_alias<T> >::type
Read(const rapidjson::Value& value, T& var)
{
    typename aliased_type<T>::type x;
    Read(value, x);
    set_aliased_value(var, x);
}


template <typename Reader>
value<void, Reader&>
MakeValue(Reader& reader, const value<void, Reader&>& element)
{
    return value<void, Reader&>(reader, element.GetRuntimeSchema());
}

template <typename Reader, typename T>
value<T, Reader&>
MakeValue(Reader& reader, const value<T, Reader&>&)
{
    return value<T, Reader&>(reader);
}

inline const std::string& FieldName(const Metadata& metadata)
{
    std::map<std::string, std::string>::const_iterator it
        = metadata.attributes.find("JsonName");

    if (it != metadata.attributes.end())
        return it->second;

    return metadata.name;
}

} // namespace detail

} // namespace bond
