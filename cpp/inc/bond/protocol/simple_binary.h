// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "encoding.h"

#include <bond/core/bond_version.h>
#include <bond/core/traits.h>

#include <boost/call_traits.hpp>
#include <boost/noncopyable.hpp>

/*
                     .-------------.----------------.
   struct            | base fields | derived fields |
                     '-------------'----------------'

                     .----------.----------.   .----------.
   fields            |  field   |  field   |...|  field   |
                     '----------'----------'   '----------'

                     .----------.
   field             |  value   |
                     '----------'

                                           .---.---.---.---.---.---.---.---.
   value            bool                   |   |   |   |   |   |   |   | v |
                                           '---'---'---'---'---'---'---'---'
                                                                          0

                    all integral types are written binary, native size, uncompressed, little endian

                    float, double          little endian


                                            .-------.------------.
                     string, wstring        | count | characters |
                                            '-------'------------'

                           count            uint32 count of 1-byte (for string)
                                            or 2-byte (for wstring) Unicode code
                                            units (variable encoded in v2)

                           characters       1-byte UTF-8 code units (for string) or 2-byte
                                            UTF-16LE code units (for wstring)


                                           .-------. .-------.
                    blob, list, set,       | count | | items |...
                    vector, nullable       '-------' '-------'

                           count            uint32 count of items (variable encoded in v2)

                           items            each item encoded according to its type

                                           .-------. .-----.--------.
                    map                    | count | | key | mapped |...
                                           '-------' '-----'--------'

                            count           uint32 count of {key,mapped} pairs (variable encoded in v2)

                            key, mapped     each item encoded according to its type

                                           .-------. .-----------.
                    bonded                 | count | | marshaled |
                                           '-------' '-----------'
                            count           uint32 count of bytes (always fixed-width, even in v2)

                            marshaled       a marshaled payload
*/

namespace bond
{


template <typename BufferT>
class SimpleBinaryWriter;


/// @brief Reader for Simple Binary protocol
template <typename BufferT, typename MarshaledBondedProtocolsT>
class SimpleBinaryReader
{
public:
    typedef BufferT                           Buffer;
    typedef StaticParser<SimpleBinaryReader&> Parser;
    typedef SimpleBinaryWriter<Buffer>        Writer;

    BOND_STATIC_CONSTEXPR uint16_t magic = SIMPLE_PROTOCOL;
    BOND_STATIC_CONSTEXPR uint16_t version = v2;


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


    /// @brief Access to underlying buffer
    typename boost::call_traits<Buffer>::const_reference
    GetBuffer() const
    {
        return _input;
    }


    /// @brief Access to underlying buffer
    typename boost::call_traits<Buffer>::reference
    GetBuffer()
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

        constexpr uint8_t charSize = static_cast<uint8_t>(sizeof(typename detail::string_char_int_type<T>::type));
        uint32_t numStringBytes = detail::checked_multiply(length, charSize);
        if (!_input.CanRead(numStringBytes))
        {
            OutOfBoundStringSizeException();
        }

        detail::ReadStringData(_input, var, length);
    }


    // Read for blob
    void Read(blob& var, uint32_t size)
    {
        _input.Read(var, size);
    }

    // Does the reader have enough input buffer left to read an array of T?
    template<typename T> 
    bool CanReadArray(uint32_t num_elems)
    {
        // We will need to read num_elems instances of T. This will not overflow because
        // num_elems < 2^32 and we call this only for primitive types, so sizeof(T) <= 8.
        uint64_t num_bytes = static_cast<uint64_t>(num_elems) * sizeof(T);

        // Check if num_bytes is 32-bit as the Reader cannot grab more than that
        return (num_bytes >> 32 == 0) && _input.CanRead(num_bytes & 0xffffffff);
    }

    template<>
    bool CanReadArray<bool>(uint32_t num_elems)
    {
        // booleans are encoded as 1 Byte
        return _input.CanRead(num_elems);
    }

    template<>
    bool CanReadArray<std::string>(uint32_t num_elems)
    {
        // This is a compile-time compare. In C++17 this problem does not exist.
#ifdef _MSC_VER
        #pragma warning(push)
        #pragma warning(disable:4127)
#endif
        BOND_IF_CONSTEXPR(version == v1)
        {
            // In v1, strings encode their length as uin32 in 4 Bytes, so we multiply num_elems.
            return _input.CanRead(detail::checked_multiply(num_elems, 4));
        }
        else
        {
            // In v2, strings use variable-length encoded integers to specify length, 1 Byte each
            // ix their minumum length each.
            return _input.CanRead(num_elems);
        }
#ifdef _MSC_VER
        #pragma warning(pop)
#endif
    }

    template<>
    bool CanReadArray<std::wstring>(uint32_t num_elems)
    {
        // This is a compile-time compare. In C++17 this problem does not exist.
        #pragma warning(push)
        #pragma warning(disable:4127)

        BOND_IF_CONSTEXPR (version == v1)
            return _input.CanRead(detail::checked_multiply(num_elems, 4));
        else
            return _input.CanRead(num_elems);

        #pragma warning(pop)
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


    template <typename Input, typename MarshaledBondedProtocols, typename Output>
    friend
    bool is_protocol_version_same(const SimpleBinaryReader<Input, MarshaledBondedProtocols>&,
                                  const SimpleBinaryWriter<Output>&);

    Buffer   _input;
    uint16_t _version;
};


template <typename BufferT, typename MarshaledBondedProtocolsT>
BOND_CONSTEXPR_OR_CONST uint16_t SimpleBinaryReader<BufferT, MarshaledBondedProtocolsT>::magic;


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

    /// @brief Access to underlying buffer
    typename boost::call_traits<Buffer>::reference
    GetBuffer()
    {
        return _output;
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

    template <typename Input, typename MarshaledBondedProtocols, typename Output>
    friend
    bool is_protocol_version_same(const SimpleBinaryReader<Input, MarshaledBondedProtocols>&,
                                  const SimpleBinaryWriter<Output>&);

    Buffer&  _output;
    uint16_t _version;
};


template <typename Input, typename MarshaledBondedProtocols> struct
protocol_has_multiple_versions<SimpleBinaryReader<Input, MarshaledBondedProtocols> >
    : std::true_type {};


template <typename Input, typename MarshaledBondedProtocols, typename Output>
bool is_protocol_version_same(const SimpleBinaryReader<Input, MarshaledBondedProtocols>& reader,
                              const SimpleBinaryWriter<Output>& writer)
{
    return reader._version == writer._version;
}

template <typename Output> struct
may_omit_fields<SimpleBinaryWriter<Output> >
    : std::false_type {};

} // namespace bond
