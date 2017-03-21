// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "encoding.h"
#include <bond/core/traits.h>
#include <bond/core/bond_version.h>
#include <boost/call_traits.hpp>
#include <boost/noncopyable.hpp>

namespace bond
{


template <typename BufferT>
class SimpleBinaryWriter;


/// @brief Reader for Simple Binary protocol
template <typename BufferT>
class SimpleBinaryReader
{
public:
    typedef BufferT                           Buffer;
    typedef StaticParser<SimpleBinaryReader&> Parser;
    typedef SimpleBinaryWriter<Buffer>        Writer;

    static const uint16_t magic; // = SIMPLE_PROTOCOL
    static const uint16_t version = v2;


    /// @brief Construct from input buffer/stream containing serialized data.
    SimpleBinaryReader(typename boost::call_traits<Buffer>::param_type input,
                       uint16_t version_value = default_version<SimpleBinaryReader>::value)
        : _input(input),
          _version(version_value)
    {
        BOOST_ASSERT(_version <= SimpleBinaryReader::version);
    }


    // This identical to compiler generated ctor except for noexcept declaration.
    // Copy ctor that is explicitly declared throw() is needed for boost::variant
    // to use optimized code path.
    /// @brief Copy constructor
    SimpleBinaryReader(const SimpleBinaryReader& that) BOND_NOEXCEPT
        : _input(that._input),
          _version(that._version)
    {}


    /// @brief Comparison operator
    bool operator==(const SimpleBinaryReader& rhs) const
    {
        return _input == rhs._input;
    }


    /// @brief Access to underlaying buffer
    typename boost::call_traits<Buffer>::const_reference
    GetBuffer() const
    {
        return _input;
    }


    bool ReadVersion()
    {
        uint16_t magic_value;

        _input.Read(magic_value);
        _input.Read(_version);

        return magic_value == SimpleBinaryReader::magic
            && _version <= SimpleBinaryReader::version;
    }


    // Read for basic types
    template <typename T>
    typename boost::disable_if<is_string_type<T> >::type
    Read(T& var)
    {
        _input.Read(var);
    }


    // Read for strings
    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Read(T& var)
    {
        uint32_t length = 0;

        ReadSize(length);
        detail::ReadStringData(_input, var, length);
    }


    // Read for blob
    void Read(blob& var, uint32_t size)
    {
        _input.Read(var, size);
    }


    // Skip for basic types
    template <typename T>
    typename boost::disable_if<is_string_type<T> >::type
    Skip()
    {
        _input.Skip(sizeof(T));
    }


    template <typename T>
    void Skip(const bonded<T, SimpleBinaryReader&>& bonded);


    // Skip for strings
    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Skip()
    {
        uint32_t length;

        ReadSize(length);
        _input.Skip(length * sizeof(typename detail::string_char_int_type<T>::type));
    }


    void Skip(BondDataType type)
    {
        switch (type)
        {
            case BT_BOOL:
            case BT_UINT8:
            case BT_INT8:
                _input.Skip(sizeof(uint8_t));
                break;

            case BT_UINT16:
            case BT_INT16:
                _input.Skip(sizeof(uint16_t));
                break;

            case BT_UINT32:
            case BT_INT32:
                _input.Skip(sizeof(uint32_t));
                break;

            case BT_UINT64:
            case BT_INT64:
                _input.Skip(sizeof(uint64_t));
                break;

            case BT_FLOAT:
                _input.Skip(sizeof(float));
                break;

            case BT_DOUBLE:
                _input.Skip(sizeof(double));
                break;

            case BT_STRING:
                Skip<std::string>();
                break;

            case BT_WSTRING:
                Skip<std::wstring>();
                break;

            default:
                break;
        }
    }


    template <typename T>
    void ReadContainerBegin(uint32_t& size, T&)
    {
        ReadSize(size);
    }

    void ReadContainerEnd()
    {}

protected:
    void ReadSize(uint32_t& size)
    {
        if (_version == v1)
            Read(size);
        else
            ReadVariableUnsigned(_input, size);
    }


    template <typename Input, typename Output>
    friend
    bool is_protocol_version_same(const SimpleBinaryReader<Input>&,
                                  const SimpleBinaryWriter<Output>&);

    Buffer   _input;
    uint16_t _version;
};


template <typename Buffer>
const uint16_t SimpleBinaryReader<Buffer>::magic = SIMPLE_PROTOCOL;


/// @brief Writer for Simple Binary protocol
template <typename BufferT>
class SimpleBinaryWriter
    : boost::noncopyable
{
public:
    typedef BufferT                     Buffer;
    typedef SimpleBinaryReader<Buffer>  Reader;

    /// @brief Construct from output buffer/stream.
    SimpleBinaryWriter(Buffer& output,
                       uint16_t version = default_version<Reader>::value)
        : _output(output),
          _version(version)
    {
        BOOST_ASSERT(_version <= Reader::version);
    }

    void WriteVersion()
    {
        _output.Write(Reader::magic);
        _output.Write(_version);
    }

    void WriteStructBegin(const Metadata& /*metadata*/, bool /*base*/)
    {}

    void WriteStructEnd(bool = false)
    {}

    void WriteFieldBegin(BondDataType /*type*/, uint16_t /*id*/, const Metadata& /*metadata*/)
    {}

    void WriteFieldBegin(BondDataType /*type*/, uint16_t /*id*/)
    {}

    void WriteFieldEnd()
    {}


    // WriteContainerBegin
    template <typename T>
    void WriteContainerBegin(uint32_t size, T)
    {
        WriteSize(size);
    }


    // WriteContainerEnd
    void WriteContainerEnd()
    {}

    template <typename T>
    void WriteField(uint16_t /*id*/, const bond::Metadata& /*metadata*/, const T& value)
    {
        Write(value);
    }

    void WriteFieldOmitted(BondDataType type, uint16_t /*id*/, const Metadata& metadata);

    // Write for basic types
    template <typename T>
    typename boost::disable_if<is_string_type<T> >::type
    Write(const T& value)
    {
        _output.Write(value);
    }

    // Write for strings
    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Write(const T& value)
    {
        uint32_t length = string_length(value);

        WriteSize(length);
        detail::WriteStringData(_output, value, length);
    }

    // Write for blob
    void Write(const blob& value)
    {
        _output.Write(value);
    }

protected:
    void WriteSize(uint32_t& size)
    {
        if (_version == v1)
            Write(size);
        else
            WriteVariableUnsigned(_output, size);
    }

    template <typename Input, typename Output>
    friend
    bool is_protocol_version_same(const SimpleBinaryReader<Input>&,
                                  const SimpleBinaryWriter<Output>&);

    Buffer&  _output;
    uint16_t _version;
};


template <typename Input> struct
protocol_has_multiple_versions<SimpleBinaryReader<Input> >
    : true_type {};


template <typename Input, typename Output>
bool is_protocol_version_same(const SimpleBinaryReader<Input>& reader,
                              const SimpleBinaryWriter<Output>& writer)
{
    return reader._version == writer._version;
}

template <typename Output> struct
may_omit_fields<SimpleBinaryWriter<Output> >
    : false_type {};

} // namespace bond
