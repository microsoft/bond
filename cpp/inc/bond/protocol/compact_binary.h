// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/simple_array.h"
#include "encoding.h"

#include <bond/core/bond_version.h>
#include <bond/core/detail/checked.h>
#include <bond/core/traits.h>
#include <bond/stream/output_counter.h>

#include <boost/call_traits.hpp>
#include <boost/noncopyable.hpp>
#include <boost/static_assert.hpp>

#include <cstring>

/*

                     .----------.--------------.   .----------.---------.
   struct (v1)       |  fields  | BT_STOP_BASE |...|  fields  | BT_STOP |
                     '----------'--------------'   '----------'---------'

                     .----------.----------.--------------.   .----------.---------.
   struct (v2)       |  length  |  fields  | BT_STOP_BASE |...|  fields  | BT_STOP |
                     '----------'----------'--------------'   '----------'---------'

   length             variable int encoded uint32 length of following fields, up to and
                      including BT_STOP but excluding length itself.

                     .----------.----------.   .----------.
   fields            |  field   |  field   |...|  field   |
                     '----------'----------'   '----------'

                     .----------.----------.
   field             | id+type  |  value   |
                     '----------'----------'

                                            .---.---.---.---.---.---.---.---.                       i - id bits (BE unsigned int)
   id+type           0 <= id <= 5           | i | i | i | t | t | t | t | t |                       t - type bits
                                            '---'---'---'---'---'---'---'---'                       v - value bits
                                              2       0   4               0

                                            .---.---.---.---.---.---.---.---.---.   .---.
                     5 < id <= 0xff         | 1 | 1 | 0 | t | t | t | t | t | i |...| i |
                                            '---'---'---'---'---'---'---'---'---'   '---'
                                                          4               0   7       0

                                            .---.---.---.---.---.---.---.---.---.   .---.---.   .---.
                     0xff < id <= 0xffff    | 1 | 1 | 1 | t | t | t | t | t | i |...| i | i |...| i |
                                            '---'---'---'---'---'---'---'---'---'   '---'---'   '---'
                                                          4               0   7       0   15      8


                                            .---.---.---.---.---.---.---.---.
   value             bool                   |   |   |   |   |   |   |   | v |
                                            '---'---'---'---'---'---'---'---'
                                                                          0

                                            .---.---.---.---.---.---.---.---.
                     int8, uint8            | v | v | v | v | v | v | v | v |
                                            '---'---'---'---'---'---'---'---'
                                              7                           0

                                            signed uses two's complement

                                            .---.---.   .---.---.---.   .---.
                     uint16, uint32,        | 1 | v |...| v | 0 | v |...| v |  [...]
                     uint64                 '---'---'   '---'---'---'   '---'
                                                  6       0       13      7

                                            LEB128 variable encoding, high bit of every byte
                                            indicates if there is another byte


                     int16, int32,          zig zag encoded to unsigned integer:
                     int64
                                             0 -> 0
                                            -1 -> 1
                                             1 -> 2
                                            -2 -> 3
                                            ...

                                            and then encoded as unsigned integer


                     float, double          32-bit or 64-bit little endian IEEE 764


                     enum                   enum constants are encoded using int32

                                            .-------.------------.
                     string, wstring        | count | characters |
                                            '-------'------------'

                           count            variable encoded uint32 count of 1-byte (for
                                            string) or 2-byte (for wstring) Unicode code
                                            units

                           characters       1-byte UTF-8 code units (for string) or 2-byte
                                            UTF-16LE code units (for wstring)


                                            .-------.-------.-------.
                     blob, list, set,       | type  | count | items |
                     vector, nullable       '-------'-------'-------'

                                            .---.---.---.---.---.---.---.---.
                           type (v1)        |   |   |   | t | t | t | t | t |
                                            '---'---'---'---'---'---'---'---'
                                                          4               0

                                            .---.---.---.---.---.---.---.---.
                           type (v2)        | c | c | c | t | t | t | t | t |
                                            '---'---'---'---'---'---'---'---'
                                              2       0   4               0

                                            if count of items is < 7, 'c' are bit of (count + 1),
                                            otherwise 'c' bits are 0.

                           count            variable encoded uint32 count of items
                                            omitted in v2 if 'c' bits within type byte are not 0

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

*/

namespace bond
{


template <typename BufferT>
class CompactBinaryWriter;

/// @brief Reader for Compact Binary Protocol
template <typename BufferT>
class CompactBinaryReader
{
public:
    typedef BufferT                             Buffer;
    typedef DynamicParser<CompactBinaryReader&> Parser;
    typedef CompactBinaryWriter<Buffer>         Writer;

    BOND_STATIC_CONSTEXPR uint16_t magic = COMPACT_PROTOCOL;
    BOND_STATIC_CONSTEXPR uint16_t version = v2;

    /// @brief Construct from input buffer/stream containing serialized data.
    CompactBinaryReader(typename boost::call_traits<Buffer>::param_type input,
                        uint16_t version_value = default_version<CompactBinaryReader>::value)
        : _input(input),
          _version(version_value)
    {
        BOOST_ASSERT(protocol_has_multiple_versions<CompactBinaryReader>::value
            ? _version <= CompactBinaryReader::version
            : _version == default_version<CompactBinaryReader>::value);
    }


    // This identical to compiler generated ctor except for noexcept declaration.
    // Copy ctor that is explicitly declared throw() is needed for boost::variant
    // to use optimized code path.
    /// @brief Copy constructor
    CompactBinaryReader(const CompactBinaryReader& that) BOND_NOEXCEPT
        : _input(that._input),
          _version(that._version)
    {}


    /// @brief Comparison operator
    bool operator==(const CompactBinaryReader& rhs) const
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

        return magic_value == CompactBinaryReader::magic
            && (protocol_has_multiple_versions<CompactBinaryReader>::value
                ? _version <= CompactBinaryReader::version
                : _version == default_version<CompactBinaryReader>::value);
    }


    // ReadStructBegin
    void ReadStructBegin(bool base = false)
    {
        if (!base && v2 == _version)
        {
            uint32_t length;
            Read(length);
        }
    }

    // ReadStructEnd
    void ReadStructEnd(bool = false)
    {}

    // ReadFieldBegin
    void ReadFieldBegin(BondDataType& type, uint16_t& id)
    {
        uint8_t raw;

        _input.Read(raw);

        type = static_cast<BondDataType>(raw & 0x1f);
        id = static_cast<uint16_t>(raw & (0x07 << 5));

        if (id == (0x07 << 5))
        {
            // ID is in (0xff, 0xffff] and is in the next two bytes
            _input.Read(id);
        }
        else if (id == (0x06 << 5))
        {
            // ID is in (5, 0xff] and is in the next one byte
            _input.Read(raw);
            id = static_cast<uint16_t>(raw);
        }
        else
        {
            // ID is in [0, 5] and was in the byte we already read
            id >>= 5;
        }
    }

    // ReadFieldEnd
    void ReadFieldEnd()
    {}


    // ReadContainerBegin
    void ReadContainerBegin(uint32_t& size, BondDataType& type)
    {
        uint8_t raw;

        _input.Read(raw);
        type = static_cast<BondDataType>(raw & 0x1f);

        if (v2 == _version && (raw & (0x07 << 5)))
            size = (raw >> 5) - 1;
        else
            Read(size);
    }


    // container of 2-tuple (e.g. map)
    void ReadContainerBegin(uint32_t& size, std::pair<BondDataType, BondDataType>& type)
    {
        uint8_t raw;

        _input.Read(raw);
        type.first = static_cast<BondDataType>(raw);

        _input.Read(raw);
        type.second = static_cast<BondDataType>(raw);

        Read(size);
    }


    // ReadContainerEnd
    void ReadContainerEnd()
    {}


    // Read for floating point
    template <typename T>
    typename boost::enable_if<std::is_floating_point<T> >::type
    Read(T& value)
    {
        _input.Read(value);
    }

    // Read for unsigned integers
    template <typename T>
    typename boost::enable_if<std::is_unsigned<T> >::type
    Read(T& value)
    {
        ReadVariableUnsigned(_input, value);
    }

    // Read for signed integers
    template <typename T>
    typename boost::enable_if<is_signed_int<T> >::type
    Read(T& value)
    {
        typename std::make_unsigned<T>::type unsigned_value;

        ReadVariableUnsigned(_input, unsigned_value);
        value = DecodeZigZag(unsigned_value);
    }


    // Read for enums
    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    Read(T& value)
    {
        BOOST_STATIC_ASSERT(sizeof(value) == sizeof(int32_t));
        int32_t raw;
        Read(raw);
        std::memcpy(&value, &raw, sizeof(raw));
    }


    // Read for int8_t
    void Read(int8_t& value)
    {
        _input.Read(value);
    }


    // Read for uint8_t
    void Read(uint8_t& value)
    {
        _input.Read(value);
    }


    // Read for bool
    void Read(bool& value)
    {
        _input.Read(value);
    }


    // Read for strings
    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Read(T& value)
    {
        uint32_t length = 0;

        Read(length);
        detail::ReadStringData(_input, value, length);
    }


    // Read for blob
    void Read(blob& value, uint32_t size)
    {
        _input.Read(value, size);
    }


    template <typename T>
    void Skip()
    {
        SkipType<get_type_id<T>::value>();
    }

    template <typename T>
    void Skip(const bonded<T, CompactBinaryReader&>&)
    {
        SkipType<bond::BT_STRUCT>();
    }

    void Skip(BondDataType type)
    {
        SkipType(type);
    }

protected:
    using BT = BondDataType;

    template <BT T>
    typename boost::enable_if_c<(T == BT_BOOL || T == BT_UINT8 || T == BT_INT8)>::type
    SkipType(uint32_t size = 1)
    {
        _input.Skip(detail::checked_multiply(size, sizeof(uint8_t)));
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_UINT16 || T == BT_UINT32 || T == BT_UINT64
                                || T == BT_INT16 || T == BT_INT32 || T == BT_INT64)>::type
    SkipType()
    {
        uint64_t value;
        Read(value);
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_FLOAT)>::type
    SkipType(uint32_t size = 1)
    {
        _input.Skip(detail::checked_multiply(size, sizeof(float)));
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_DOUBLE)>::type
    SkipType(uint32_t size = 1)
    {
        _input.Skip(detail::checked_multiply(size, sizeof(double)));
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_STRING)>::type
    SkipType()
    {
        uint32_t length;

        Read(length);
        _input.Skip(length);
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_WSTRING)>::type
    SkipType()
    {
        uint32_t length;

        Read(length);
        _input.Skip(detail::checked_multiply(length, sizeof(uint16_t)));
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_SET || T == BT_LIST)>::type
    SkipType()
    {
        BondDataType element_type;
        uint32_t     size;

        ReadContainerBegin(size, element_type);
        SkipType(element_type, size);
        ReadContainerEnd();
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_MAP)>::type
    SkipType()
    {
        std::pair<BondDataType, BondDataType>   element_type;
        uint32_t                                size;

        ReadContainerBegin(size, element_type);
        for (int64_t i = 0; i < size; ++i)
        {
            SkipType(element_type.first);
            SkipType(element_type.second);
        }
        ReadContainerEnd();
    }

    void SkipStructV1()
    {
        BOOST_ASSERT(v1 == _version);

        for (;;)
        {
            ReadStructBegin();

            uint16_t     id;
            BondDataType field_type;

            for (ReadFieldBegin(field_type, id);
                    field_type != bond::BT_STOP && field_type != bond::BT_STOP_BASE;
                    ReadFieldEnd(), ReadFieldBegin(field_type, id))
            {
                SkipType(field_type);
            }

            ReadStructEnd();

            if (field_type == bond::BT_STOP)
                break;
        }
    }

    void SkipStructV2()
    {
        BOOST_ASSERT(v2 == _version);

        uint32_t length;
        Read(length);
        _input.Skip(length);
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_STRUCT)>::type
    SkipType()
    {
        if (v2 == _version)
        {
            SkipStructV2();
        }
        else
        {
            SkipStructV1();
        }
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_STRUCT)>::type
    SkipType(uint32_t size)
    {
        if (v2 == _version)
        {
            for (int64_t i = 0; i < size; ++i)
            {
                SkipStructV2();
            }
        }
        else
        {
            for (int64_t i = 0; i < size; ++i)
            {
                SkipStructV1();
            }
        }
    }

    template <BT T>
    typename boost::enable_if_c<(T == BT_UINT16 || T == BT_UINT32 || T == BT_UINT64
                                || T == BT_INT16 || T == BT_INT32 || T == BT_INT64
                                || T == BT_STRING || T == BT_WSTRING
                                || T == BT_SET || T == BT_LIST || T == BT_MAP)>::type
    SkipType(uint32_t size)
    {
        for (int64_t i = 0; i < size; ++i)
        {
            SkipType<T>();
        }
    }

    template <typename... Args>
    void SkipType(BondDataType type, Args&&... args)
    {
        switch (type)
        {
            case BT_BOOL:
            case BT_UINT8:
            case BT_INT8:
                SkipType<BT_BOOL>(std::forward<Args>(args)...);
                break;

            case BT_UINT64:
            case BT_UINT32:
            case BT_UINT16:
            case BT_INT64:
            case BT_INT32:
            case BT_INT16:
                SkipType<BT_UINT64>(std::forward<Args>(args)...);
                break;

            case BT_FLOAT:
                SkipType<BT_FLOAT>(std::forward<Args>(args)...);
                break;

            case BT_DOUBLE:
                SkipType<BT_DOUBLE>(std::forward<Args>(args)...);
                break;

            case BT_STRING:
                SkipType<BT_STRING>(std::forward<Args>(args)...);
                break;

            case BT_WSTRING:
                SkipType<BT_WSTRING>(std::forward<Args>(args)...);
                break;

            case BT_SET:
            case BT_LIST:
                SkipType<BT_SET>(std::forward<Args>(args)...);
                break;

            case BT_MAP:
                SkipType<BT_MAP>(std::forward<Args>(args)...);
                break;

            case BT_STRUCT:
                SkipType<BT_STRUCT>(std::forward<Args>(args)...);
                break;

            default:
                break;
        }
    }

    Buffer  _input;
    uint16_t _version;

    template <typename Input, typename Output>
    friend
    bool is_protocol_version_same(const CompactBinaryReader<Input>&,
                                  const CompactBinaryWriter<Output>&);
};

template <typename BufferT>
BOND_CONSTEXPR_OR_CONST uint16_t CompactBinaryReader<BufferT>::magic;


class CompactBinaryCounter
{
    template <typename Buffer>
    friend class CompactBinaryWriter;

private:
    struct type : OutputCounter // Must be a new type and not an alias.
    {};
};


/// @brief Writer for Compact Binary Protocol
template <typename BufferT>
class CompactBinaryWriter
    : boost::noncopyable
{
    struct Pass1
    {
        Pass1(CompactBinaryWriter* writer)
            : writer(writer)
        {}

        ~Pass1()
        {
            writer->_it = NULL;
        }

        CompactBinaryWriter* writer;
    };

    using Counter = CompactBinaryCounter::type;

public:
    typedef BufferT                         Buffer;
    typedef CompactBinaryReader<Buffer>     Reader;
    typedef CompactBinaryWriter<Counter>    Pass0;


    /// @brief Construct from output buffer/stream.
    CompactBinaryWriter(Buffer& output,
                        uint16_t version = default_version<Reader>::value)
        : _output(output),
          _it(NULL),
          _version(version)
    {
        BOOST_ASSERT(protocol_has_multiple_versions<Reader>::value
            ? _version <= Reader::version
            : _version == default_version<Reader>::value);
    }

    template<typename T>
    CompactBinaryWriter(Counter& output,
                        const CompactBinaryWriter<T>& pass1)
        : _output(output),
          _version(pass1._version)
    {}


    /// @brief Access to underlying buffer
    typename boost::call_traits<Buffer>::reference
    GetBuffer()
    {
        return _output;
    }


    bool NeedPass0()
    {
        return v2 == _version && !_it;
    }


    Pass1 WithPass0(Pass0& pass0)
    {
        _it = pass0._lengths.begin();
        return this;
    }

    void WriteVersion()
    {
        _output.Write(Reader::magic);
        _output.Write(_version);
    }


    void WriteStructBegin(const Metadata& /*metadata*/, bool base)
    {
        if (!base)
        {
            LengthBegin(_output);
        }
    }

    void WriteStructEnd(bool base = false)
    {
        if (base)
        {
            _output.Write(static_cast<uint8_t>(BT_STOP_BASE));
        }
        else
        {
            _output.Write(static_cast<uint8_t>(BT_STOP));
            LengthEnd(_output);
        }
    }

    // WriteField for basic types
    template <typename T>
    void WriteField(uint16_t id, const bond::Metadata& /*metadata*/, const T& value)
    {
        WriteFieldBegin(get_type_id<T>::value, id);
        Write(value);
        WriteFieldEnd();
    }

    // WriteFieldBegin
    void WriteFieldBegin(BondDataType type, uint16_t id, const ::bond::Metadata& /*metadata*/)
    {
        WriteFieldBegin(type, id);
    }

    void WriteFieldBegin(BondDataType type, uint16_t id)
    {
        BOOST_ASSERT((type & 0x1f) == type);

        if (id <= 5)
        {
            _output.Write(static_cast<uint8_t>(type | ((id) << 5)));
        }
        else if (id <= 0xff)
        {
            _output.Write(static_cast<uint8_t>(type | (0x06 << 5)));
            _output.Write(static_cast<uint8_t>(id));
        }
        else
        {
            _output.Write(static_cast<uint8_t>(type | (0x07 << 5)));
            _output.Write(id);
        }
    }

    // WriteFieldEnd
    void WriteFieldEnd()
    {}

    // WriteContainerBegin
    void WriteContainerBegin(uint32_t size, BondDataType type)
    {
        BOOST_ASSERT((type & 0x1f) == type);

        if (v2 == _version && size < 7)
        {
            Write(static_cast<uint8_t>(type | ((size + 1) << 5)));
        }
        else
        {
            Write(static_cast<uint8_t>(type));
            Write(size);
        }
    }

    // container of 2-tuples (e.g. map)
    void WriteContainerBegin(uint32_t size, std::pair<BondDataType, BondDataType> type)
    {
        Write(static_cast<uint8_t>(type.first));
        Write(static_cast<uint8_t>(type.second));
        WriteVariableUnsigned(_output, size);
    }

    // WriteContainerEnd
    void WriteContainerEnd()
    {}

    // Write for floating point
    template <typename T>
    typename boost::enable_if<std::is_floating_point<T> >::type
    Write(const T& value)
    {
        _output.Write(value);
    }

    // Write for unsigned integers
    template <typename T>
    typename boost::enable_if<std::is_unsigned<T> >::type
    Write(const T& value)
    {
        WriteVariableUnsigned(_output, value);
    }

    // Write for signed integers
    template <typename T>
    typename boost::enable_if<is_signed_int<T> >::type
    Write(const T& value)
    {
        WriteVariableUnsigned(_output, EncodeZigZag(value));
    }

    // Write for enums
    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    Write(const T& value)
    {
        BOOST_STATIC_ASSERT(sizeof(value) == sizeof(int32_t));
        Write(static_cast<int32_t>(value));
    }

    // Write for int8_t
    void Write(const int8_t& value)
    {
        _output.Write(value);
    }

    // Write for uint8_t
    void Write(const uint8_t& value)
    {
        _output.Write(value);
    }

    // Write for bool
    void Write(const bool& value)
    {
        _output.Write(value);
    }

    // Write for strings
    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Write(const T& value)
    {
        uint32_t length = string_length(value);

        Write(length);
        detail::WriteStringData(_output, value, length);
    }

    // Write for blob
    void Write(const blob& value)
    {
        _output.Write(value);
    }

protected:
    template <typename Buffer>
    friend class CompactBinaryWriter;

    void LengthBegin(Counter& counter)
    {
        _stack.push(_lengths.size());
        _lengths.push(counter.GetCount());
    }

    void LengthEnd(Counter& counter)
    {
        uint32_t& length = _lengths[_stack.pop()];

        length = counter.GetCount() - length;
        counter.WriteVariableUnsigned(length);
    }

    template<typename T>
    void LengthBegin(T&)
    {
        if (v2 == _version)
        {
            Write(*_it++);
        }
    }

    template<typename T>
    void LengthEnd(T&)
    {}

protected:
    Buffer&                         _output;
    const uint32_t*                 _it;
    uint16_t                        _version;
    detail::SimpleArray<uint32_t>   _stack;
    detail::SimpleArray<uint32_t>   _lengths;

    template <typename Input, typename Output>
    friend
    bool is_protocol_version_same(const CompactBinaryReader<Input>&,
                                  const CompactBinaryWriter<Output>&);
};

template <typename Input> struct
protocol_has_multiple_versions<CompactBinaryReader<Input> >
    : enable_protocol_versions<CompactBinaryReader<Input> > {};

template <typename Input, typename Output>
inline
bool is_protocol_version_same(const CompactBinaryReader<Input>& reader,
                              const CompactBinaryWriter<Output>& writer)
{
    return reader._version == writer._version;
}

} // namespace bond
