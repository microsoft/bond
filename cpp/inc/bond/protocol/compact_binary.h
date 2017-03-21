// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "encoding.h"
#include "detail/simple_array.h"
#include <bond/core/bond_version.h>
#include <bond/core/traits.h>
#include <bond/stream/output_counter.h>
#include <boost/call_traits.hpp>
#include <boost/noncopyable.hpp>

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

                                            .---.---.---.---.---.---.---.---.                       i - id bits
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

                                            .---.---.   .---.---.---.   .---.
                     uint16, uint32,        | 1 | v |...| v | 0 | v |...| v |  [...]
                     uint64                 '---'---'   '---'---'---'   '---'
                                                  6       0       13      7

                                            variable encoding, high bit of every byte
                                            indicates if there is another byte


                     int16, int32,          zig zag encoded to unsigned integer:
                     int64
                                             0 -> 0
                                            -1 -> 1
                                             1 -> 2
                                            -2 -> 3
                                            ...

                                            and then encoded as unsigned integer


                     float, double          little endian


                                            .-------.------------.
                     string, wstring        | count | characters |
                                            '-------'------------'

                           count            variable encoded uint32 count of 1-byte or 2-byte characters

                           characters       1-byte or 2-byte characters


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

    static const uint16_t magic; // = COMPACT_PROTOCOL
    static const uint16_t version = v2;

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
            _input.Read(id);
        }
        else if (id == (0x06 << 5))
        {
            _input.Read(reinterpret_cast<uint8_t&>(id));
        }
        else
        {
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
    typename boost::enable_if<is_floating_point<T> >::type
    Read(T& value)
    {
        _input.Read(value);
    }

    // Read for unsigned integers
    template <typename T>
    typename boost::enable_if<is_unsigned<T> >::type
    Read(T& value)
    {
        ReadVariableUnsigned(_input, value);
    }

    // Read for signed integers
    template <typename T>
    typename boost::enable_if<is_signed_int<T> >::type
    Read(T& value)
    {
        typename make_unsigned<T>::type unsigned_value;

        ReadVariableUnsigned(_input, unsigned_value);
        value = DecodeZigZag(unsigned_value);
    }


    // Read for enums
    template <typename T>
    typename boost::enable_if<is_enum<T> >::type
    Read(T& value)
    {
        BOOST_STATIC_ASSERT(sizeof(value) == sizeof(int32_t));
        Read(*reinterpret_cast<int32_t*>(&value));
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
        Skip(get_type_id<T>::value);
    }

    template <typename T>
    void Skip(const bonded<T, CompactBinaryReader&>&)
    {
        SkipComplex(bond::BT_STRUCT);
    }

    void Skip(BondDataType type)
    {
        switch (type)
        {
            case bond::BT_FLOAT:
                _input.Skip(sizeof(float));
                break;

            case bond::BT_DOUBLE:
                _input.Skip(sizeof(double));
                break;

            case bond::BT_BOOL:
            case bond::BT_UINT8:
            case bond::BT_INT8:
                _input.Skip(sizeof(uint8_t));
                break;

            case bond::BT_UINT64:
            case bond::BT_UINT32:
            case bond::BT_UINT16:
            case bond::BT_INT64:
            case bond::BT_INT32:
            case bond::BT_INT16:
            {
                uint64_t value;
                Read(value);
                break;
            }
            default:
                SkipComplex(type);
                break;
        }
    }

protected:
    void SkipComplex(BondDataType type)
    {
        switch (type)
        {
            case bond::BT_STRING:
            {
                uint32_t length;

                Read(length);
                _input.Skip(length);
                break;
            }
            case bond::BT_WSTRING:
            {
                uint32_t length;

                Read(length);
                _input.Skip(length * sizeof(uint16_t));
                break;
            }
            case bond::BT_SET:
            case bond::BT_LIST:
            {
                BondDataType element_type;
                uint32_t     size;

                ReadContainerBegin(size, element_type);
                for(uint32_t i = 0; i < size; ++i)
                {
                    Skip(element_type);
                }
                ReadContainerEnd();
                break;
            }
            case bond::BT_MAP:
            {
                std::pair<BondDataType, BondDataType>   element_type;
                uint32_t                                size;

                ReadContainerBegin(size, element_type);
                for(uint32_t i = 0; i < size; ++i)
                {
                    Skip(element_type.first);
                    Skip(element_type.second);
                }
                ReadContainerEnd();
                break;
            }
            case bond::BT_STRUCT:
            {
                if (v2 == _version)
                {
                    uint32_t length;
                    Read(length);
                    _input.Skip(length);
                }
                else for(;;)
                {
                    ReadStructBegin();

                    uint16_t     id;
                    BondDataType field_type;

                    for (ReadFieldBegin(field_type, id);
                         field_type != bond::BT_STOP && field_type != bond::BT_STOP_BASE;
                         ReadFieldEnd(), ReadFieldBegin(field_type, id))
                    {
                        Skip(field_type);
                    }

                    ReadStructEnd();

                    if (field_type == bond::BT_STOP)
                        break;
                }

                break;
            }
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

template <typename Buffer>
const uint16_t CompactBinaryReader<Buffer>::magic = COMPACT_PROTOCOL;


class OutputCounter;


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

public:
    typedef BufferT                             Buffer;
    typedef CompactBinaryReader<Buffer>         Reader;
    typedef CompactBinaryWriter<OutputCounter>  Pass0;


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
    CompactBinaryWriter(OutputCounter& output,
                        const CompactBinaryWriter<T>& pass1)
        : _output(output),
          _version(pass1._version)
    {}


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
    typename boost::enable_if<is_floating_point<T> >::type
    Write(const T& value)
    {
        _output.Write(value);
    }

    // Write for unsigned integers
    template <typename T>
    typename boost::enable_if<is_unsigned<T> >::type
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
    typename boost::enable_if<is_enum<T> >::type
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

    void LengthBegin(OutputCounter& counter)
    {
        _stack.push(_lengths.size());
        _lengths.push(counter.GetCount());
    }

    void LengthEnd(OutputCounter& counter)
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
