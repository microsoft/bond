// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "encoding.h"
#include <bond/core/bond_version.h>
#include <boost/call_traits.hpp>
#include <boost/noncopyable.hpp>

/*
                     .----------.--------------.   .----------.---------.
   struct hierarchy  |  struct  | BT_STOP_BASE |...|  struct  | BT_STOP |
                     '----------'--------------'   '----------'---------'

                     .----------.----------.   .----------.
   struct            |  field   |  field   |...|  field   |
                     '----------'----------'   '----------'

                     .------.----.----------.
   field             | type | id |  value   |
                     '------'----'----------'

                     .---.---.---.---.---.---.---.---.                           i - id bits
   type              | 0 | 0 | 0 | t | t | t | t | t |                           t - type bits
                     '---'---'---'---'---'---'---'---'                           v - value bits
                                   4               0

   id                .---.   .---.---.   .---.
                     | i |...| i | i |...| i |
                     '---'   '---'---'   '---'
                       7       0   15      8

                                            .---.---.---.---.---.---.---.---.
   value             bool                   |   |   |   |   |   |   |   | v |
                                            '---'---'---'---'---'---'---'---'
                                                                          0

                     integer,               little endian
                     float, double

                                            .-------.------------.
                     string, wstring        | count | characters |
                                            '-------'------------'

                           count            variable uint32 count of 1-byte or 2-byte characters

                           characters       1-byte or 2-byte characters


                                            .-------.-------.-------.
                     blob, list, set,       | type  | count | items |
                     vector, nullable       '-------'-------'-------'

                                            .---.---.---.---.---.---.---.---.
                           type             |   |   |   | t | t | t | t | t |
                                            '---'---'---'---'---'---'---'---'
                                                          4               0

                           count            variable uint32 count of items

                           items            each item encoded according to its type


                                            .----------.------------.-------.-----.-------.
                     map                    | key type | value type | count | key | value |
                                            '----------'------------'-------'-----'-------'

                                            .---.---.---.---.---.---.---.---.
                            key type,       |   |   |   | t | t | t | t | t |
                            value type      '---'---'---'---'---'---'---'---'
                                                          4               0

                            count           variable encoded uint32 count of {key,mapped} pairs

                            key, mapped     each item encoded according to its type


   variable uint32
                     .---.---.   .---..---.---.   .---..---.---.   .---..---.---.   .---..---.---.---.---.   .---.
                     | 1 | v |...| v || 1 | v |...| v || 1 | v |...| v || 1 | v |...| v || 0 | 0 | 0 | v |...| v |
                     '---'---'   '---''---'---'   '---''---'---'   '---''---'---'   '---''---'---'---'---'   '---'
                           6       0        13      7        20      14       27      21               31      28

                     1 to 5 bytes, high bit of every byte indicates if there is another byte

*/

namespace bond
{

template <typename BufferT>
class FastBinaryWriter;

/// @brief Reader for Fast Binary protocol
template <typename BufferT>
class FastBinaryReader
{
public:
    typedef BufferT                             Buffer;
    typedef DynamicParser<FastBinaryReader&>   Parser;
    typedef FastBinaryWriter<Buffer>           Writer;

    static const uint16_t magic; // = FAST_PROTOCOL
    static const uint16_t version; // = v1

    /// @brief Construct from input buffer/stream containing serialized data.
    FastBinaryReader(typename boost::call_traits<Buffer>::param_type buffer)
       : _input(buffer)
    {}


    // This identical to compiler generated ctor except for noexcept declaration.
    // Copy ctor that is explicitly declared throw() is needed for boost::variant
    // to use optimized code path.
    /// @brief Copy constructor
    FastBinaryReader(const FastBinaryReader& that) BOND_NOEXCEPT
        : _input(that._input)
    {}


    /// @brief Comparison operator
    bool operator==(const FastBinaryReader& rhs) const
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
        uint16_t magic_value, version_value;

        _input.Read(magic_value);
        _input.Read(version_value);

        return magic_value == FastBinaryReader::magic
            && version_value <= FastBinaryReader::version;
    }


    // Read for primitive types
    template <typename T>
    typename boost::disable_if<is_string_type<T> >::type
    Read(T& value)
    {
        _input.Read(value);
    }


    // Read for strings
    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Read(T& value)
    {
        uint32_t length = 0;

        ReadVariableUnsigned(_input, length);
        detail::ReadStringData(_input, value, length);
    }


    // Read for blob
    void Read(blob& value, uint32_t size)
    {
        _input.Read(value, size);
    }

    void ReadStructBegin()
    {}


    void ReadStructEnd()
    {}


    void ReadFieldBegin(BondDataType& type, uint16_t& id)
    {
        ReadType(type);

        if (type != BT_STOP && type != BT_STOP_BASE)
            Read(id);
        else
            id = 0;
    }


    void ReadFieldEnd()
    {}


    void ReadContainerBegin(uint32_t& size, BondDataType& type)
    {
        ReadType(type);
        ReadVariableUnsigned(_input, size);
    }


    // container of 2-tuple (e.g. map)
    void ReadContainerBegin(uint32_t& size, std::pair<BondDataType, BondDataType>& type)
    {
        ReadType(type.first);
        ReadType(type.second);
        ReadVariableUnsigned(_input, size);
    }


    void ReadContainerEnd()
    {}


    template <typename T>
    void Skip()
    {
        Skip(get_type_id<T>::value);
    }


    template <typename T>
    void Skip(const bonded<T, FastBinaryReader&>&)
    {
        SkipComplex(BT_STRUCT);
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

            default:
                SkipComplex(type);
                break;
        }
    }

protected:
    void ReadType(BondDataType& type)
    {
        uint8_t byte;

        Read(byte);
        type = static_cast<BondDataType>(byte);
    }

    void SkipComplex(BondDataType type)
    {
        switch (type)
        {
            case BT_STRING:
            {
                uint32_t size = 0;

                ReadVariableUnsigned(_input, size);
                _input.Skip(size);
                break;
            }
            case BT_WSTRING:
            {
                uint32_t size = 0;

                ReadVariableUnsigned(_input, size);
                _input.Skip(size * sizeof(uint16_t));
                break;
            }
            case BT_STRUCT:
            {
                for(;;)
                {
                    ReadStructBegin();

                    uint16_t     id;
                    BondDataType field_type;

                    for (ReadFieldBegin(field_type, id);
                            field_type != BT_STOP && field_type != BT_STOP_BASE;
                            ReadFieldEnd(), ReadFieldBegin(field_type, id))
                    {
                        Skip(field_type);
                    }

                    ReadStructEnd();

                    if (field_type == BT_STOP)
                        break;
                }

                break;
            }
            case BT_SET:
            case BT_LIST:
            {
                BondDataType element_type;
                uint32_t size;

                ReadContainerBegin(size, element_type);

                for(uint32_t i = 0; i < size; i++)
                {
                    Skip(element_type);
                }

                ReadContainerEnd();
                break;
            }
            case BT_MAP:
            {
                std::pair<BondDataType, BondDataType> element_type;
                uint32_t size;

                ReadContainerBegin(size, element_type);

                for(uint32_t i = 0; i < size; i++)
                {
                    Skip(element_type.first);
                    Skip(element_type.second);
                }

                ReadContainerEnd();
                break;
            }
            default:
                break;
        }
    }

    Buffer _input;
};

template <typename Buffer>
const uint16_t FastBinaryReader<Buffer>::magic = FAST_PROTOCOL;

template <typename Buffer>
const uint16_t FastBinaryReader<Buffer>::version = v1;


/// @brief Writer for Fast Binary protocol
template <typename BufferT>
class FastBinaryWriter
    : boost::noncopyable
{
public:
    typedef BufferT                     Buffer;
    typedef FastBinaryReader<Buffer>   Reader;

    /// @brief Construct from output buffer/stream.
    FastBinaryWriter(Buffer& buffer)
        : _output(buffer)
    {
    }

    void WriteVersion()
    {
        _output.Write(Reader::magic);
        _output.Write(Reader::version);
    }

    //
    // Write methods
    //
    void WriteStructBegin(const Metadata& /*metadata*/, bool /*base*/)
    {}

    void WriteStructEnd(bool base = false)
    {
        WriteType(base ? BT_STOP_BASE : BT_STOP);
    }

    template <typename T>
    void WriteField(uint16_t id, const bond::Metadata& /*metadata*/, const T& value)
    {
        WriteFieldBegin(get_type_id<T>::value, id);
        Write(value);
        WriteFieldEnd();
    }

    void WriteFieldBegin(BondDataType type, uint16_t id, const bond::Metadata& /*metadata*/)
    {
        WriteFieldBegin(type, id);
    }

    void WriteFieldBegin(BondDataType type, uint16_t id)
    {
        WriteType(type);
        Write(id);
    }

    void WriteFieldEnd()
    {}

    void WriteContainerBegin(uint32_t size, BondDataType type)
    {
        WriteType(type);
        WriteVariableUnsigned(_output, size);
    }

    // container of 2-tuples (e.g. map)
    void WriteContainerBegin(uint32_t size, std::pair<BondDataType, BondDataType> type)
    {
        WriteType(type.first);
        WriteType(type.second);
        WriteVariableUnsigned(_output, size);
    }

    void WriteContainerEnd()
    {}

    // Write for primitive types
    template<typename T>
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

        WriteVariableUnsigned(_output, length);
        detail::WriteStringData(_output, value, length);
    }

    // Write for blob
    void Write(const blob& value)
    {
        _output.Write(value);
    }

protected:
    void WriteType(BondDataType type)
    {
        _output.Write(static_cast<uint8_t>(type));
    }

    Buffer& _output;
};


} // namespace bond

