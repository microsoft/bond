// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/rapidjson_helper.h"
#include "encoding.h"

#include <bond/core/transforms.h>

namespace bond
{


template <typename Buffer>
class SimpleJsonReader;


/// @brief Writer for Simple JSON
template <typename BufferT>
class SimpleJsonWriter
    : protected rapidjson::Writer<detail::RapidJsonOutputStream<BufferT> >,
      boost::noncopyable
{
public:
    typedef BufferT                     Buffer;
    typedef SimpleJsonReader<Buffer>    Reader;

    /// @brief Construct from output buffer/stream.
    /// @param output reference to output buffer/stream
    /// @param pretty true to generate output with whitespaces, default false
    /// @param indent number of spaces of indentation, default 4
    /// @param all_fields if false, optional fields may be omitted, default true
    SimpleJsonWriter(Buffer& output, bool pretty = false, int indent = 4, bool all_fields = true)
        : rapidjson::Writer<detail::RapidJsonOutputStream<BufferT> >(_stream),
          _stream(output),
          _output(output),
          _count(0),
          _level(0),
          _indent((std::min)(indent, 8)),
          _pretty(pretty),
          _all_fields(all_fields)
    {}

    /// @brief Access to underlying buffer
    typename boost::call_traits<Buffer>::reference
    GetBuffer()
    {
        return _output;
    }

    void WriteVersion()
    {}
    
    void WriteOpen(char bracket)
    {
        _output.Write(bracket);
        ++_level;
        _count = 0;
    }

    void WriteClose(char bracket)
    {
        --_level;
        ++_count;
        NewLine();
        _output.Write(bracket);
    }

    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    WriteName(const T& name)
    {
        WriteString(name);
        _output.Write(": ", _pretty ? 2 : 1);
    }
    
    void WriteName(uint16_t id)
    {
        _output.Write('\"');
        this->WriteUint(id);
        _output.Write("\": ", _pretty ? 3 : 2);
    }
    
    void Write(bool value)
    {
        if (value)
            _output.Write("true", 4);
        else
            _output.Write("false", 5);
    }

    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Write(const T& value)
    {
        WriteString(value);
    }

    template <typename T>
    typename boost::enable_if<is_signed_int<T> >::type
    Write(T value)
    {
        this->WriteInt64(value);
    }

    template <typename T>
    typename boost::enable_if<std::is_unsigned<T> >::type
    Write(T value)
    {
        this->WriteUint64(value);
    }

    void Write(double value)
    {
        this->WriteDouble(value);
    }

    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    Write(const T& value)
    {
        this->WriteInt(static_cast<int>(value));
    }

    template <typename T>
    typename boost::enable_if<is_type_alias<T> >::type
    Write(const T& value)
    {
        Write(get_aliased_value(value));
    }

    void WriteNull()
    {
        _output.Write("null", 4);
    }

    void WriteSeparator(const int per_line = 1)
    {
        if (_count)
            _output.Write(", ", _pretty ? 2 : 1);
        
        if (_count++ % per_line == 0)
            NewLine();
    }
    
private:
    using rapidjson::Writer<detail::RapidJsonOutputStream<BufferT> >::WriteString;

    template <typename T>
    typename boost::enable_if<is_string<T> >::type
    WriteString(const T& value)
    {
        WriteString(string_data(value), string_length(value));
    }

    template <typename T>
    typename boost::enable_if<is_wstring<T> >::type
    WriteString(const T& value)
    {
        _output.Write('\"');
        for (const wchar_t *p = string_data(value), *end = p + string_length(value); p < end; ++p) 
        {
            wchar_t c = *p;

            if (c < L'\x20' || c == '"' || c == '\\' || c == '/')
            {
                switch (c)
                {
                    case L'\b': c = L'b'; break;
                    case L'\f': c = L'f'; break;
                    case L'\n': c = L'n'; break;
                    case L'\r': c = L'r'; break;
                    case L'\t': c = L't'; break;
                }

                if (c >= L'\x20')
                    _output.Write('\\');
            }
            
            if (c >= L'\x20' && c < L'\x80')
            {
                _output.Write(static_cast<char>(c));
            }
            else
            {
                WriteUnicode(c);
            }
        }
        _output.Write('\"');
    }

    void WriteUnicode(wchar_t c)
    {
        char u[6] = "\\u";
        u[2] = detail::HexDigit(static_cast<int>(c >> 12));
        u[3] = detail::HexDigit(static_cast<int>(c >> 8));
        u[4] = detail::HexDigit(static_cast<int>(c >> 4));
        u[5] = detail::HexDigit(static_cast<int>(c >> 0));
        _output.Write(u, sizeof(u));
    }

    void NewLine()
    {
        if (!_pretty)
            return;
        
        _output.Write('\n');
        for (int i = _level; i--;)
            _output.Write("        ", _indent);
    }

    template <typename Writer, typename Protocols>
    friend class Serializer;
    
    detail::RapidJsonOutputStream<BufferT> _stream;
    Buffer& _output;
    int _count;
    int _level;
    const int _indent;
    const bool _pretty;
    const bool _all_fields;
};


template <typename Buffer> struct 
is_writer<SimpleJsonWriter<Buffer>, void>
    : std::true_type {};


template <typename Buffer, typename Protocols>
class Serializer<SimpleJsonWriter<Buffer>, Protocols>
    : public SerializingTransform
{
public:
    typedef SimpleJsonWriter<Buffer> writer_type;

    Serializer(writer_type& writer)
        : _output(writer),
          _level(0)
    {}

    void Begin(const Metadata& /*metadata*/) const
    {
        if (!_level++)
            _output.WriteOpen('{');
    }

    void End() const
    {
        WriteEnd(--_level != 0);
    }

    void UnknownEnd() const
    {
        WriteEnd(true);
    }

    template <typename T>
    bool Base(const T& value) const
    {
        Apply<Protocols>(*this, value);
        return false;
    }

    template <typename T>
    bool Field(uint16_t /*id*/, const Metadata& metadata, const maybe<T>& value) const
    {
        if (!value.is_nothing())
        {
            WriteName(detail::FieldName(metadata));
            Write(value.value());
        }
        return false;
    }
    
    template <typename T>
    bool Field(uint16_t /*id*/, const Metadata& metadata, const T& value) const
    {
        if (_output._all_fields
         || !detail::omit_field<writer_type>(metadata, value))
        {
            WriteName(detail::FieldName(metadata));
            Write(value);
        }
        return false;
    }

    template <typename T>
    bool UnknownField(uint16_t id, const T& value) const
    {
        WriteName(id);
        Write(value);
        return false;
    }

    bool OmittedField(uint16_t id, const Metadata& metadata, BondDataType type) const
    {
        if (_output._all_fields && !metadata.default_value.nothing)
        {
            switch (type)
            {
                case BT_BOOL:
                    Field(id, metadata, !!metadata.default_value.uint_value);
                    break;
                case BT_UINT8:
                case BT_UINT16:
                case BT_UINT32:
                case BT_UINT64:
                    Field(id, metadata, metadata.default_value.uint_value);
                    break;
                case BT_FLOAT:
                case BT_DOUBLE:
                    Field(id, metadata, metadata.default_value.double_value);
                    break;
                case BT_STRING:
                    Field(id, metadata, metadata.default_value.string_value);
                    break;
                case BT_STRUCT:
                    BOOST_ASSERT(false);
                    break;
                case BT_LIST:
                case BT_SET:
                    WriteName(detail::FieldName(metadata));
                    _output.WriteOpen('[');
                    _output.WriteClose(']');
                    break;
                case BT_MAP:
                    WriteName(detail::FieldName(metadata));
                    _output.WriteOpen('{');
                    _output.WriteClose('}');
                    break;
                case BT_INT8:
                case BT_INT16:
                case BT_INT32:
                case BT_INT64:
                    Field(id, metadata, metadata.default_value.int_value);
                    break;
                case BT_WSTRING:
                    Field(id, metadata, metadata.default_value.wstring_value);
                    break;
                default:
                    BOOST_ASSERT(false);
                    break;
            }
        }

        return false;
    }

    template <typename T, typename Reader>
    void Container(const value<T, Reader>& element, uint32_t size) const
    {    
        _output.WriteOpen('[');

        while (size--)
        {
            _output.WriteSeparator();
            Write(element);
        }

        _output.WriteClose(']');
    }

    template <typename Key, typename T, typename Reader>
    void Container(const value<Key, Reader>& key, const T& value, uint32_t size) const
    {    
        _output.WriteOpen('[');

        while (size--)
        {
            _output.WriteSeparator();
            Write(key);
            _output.WriteSeparator();
            Write(value);
        }
    
        _output.WriteClose(']');
    }

private:
    void WriteEnd(bool base) const
    {
        if (!base)
            _output.WriteClose('}');
    }

    template <typename T>
    void WriteName(const T& name) const
    {
        _output.WriteSeparator();
        _output.WriteName(name);
    }

    // basic, non-enum type value
    template <typename T>
    typename boost::enable_if<is_basic_type<T> >::type
    Write(const T& value) const
    {
        _output.Write(value);
    }

    // nullable<T> value
    template <typename T>
    void Write(const nullable<T>& value) const
    {
        if (!value)
        {
            _output.WriteNull();
        }
        else
        {
            _output.WriteOpen('[');
            Write(value.value());
            _output.WriteClose(']');
        }
    }

    // struct or bonded<T>
    template <typename T>
    typename boost::enable_if<is_bond_type<T> >::type
    Write(const T& value) const
    {
        Apply<Protocols>(SerializeTo<Protocols>(_output), value);
    }

    // 2-tuple
    template <typename T1, typename T2>
    void Write(const std::pair<T1, T2>& value) const
    {
        Write(value.first);
        _output.WriteSeparator(is_basic_type<T2>::value ? 2 : 1);
        Write(value.second);
    }

    // container value
    template <typename T>
    typename boost::enable_if<is_container<T> >::type
    Write(const T& value) const
    {
        _output.WriteOpen('[');

        for (const_enumerator<T> elements(value); elements.more();)
        {
            _output.WriteSeparator();
            Write(elements.next());
        }

        _output.WriteClose(']');
    }

    // blob
    void Write(const blob& value) const
    {
        _output.WriteOpen('[');

        for (const char& ch : value)
        {
            _output.WriteSeparator();
            _output.Write(static_cast<int8_t>(ch));
        }

        _output.WriteClose(']');
    }

    // serialized value
    template <typename Reader, typename T>
    typename boost::enable_if<is_basic_type<T> >::type
    Write(const value<T, Reader>& value) const
    {
        T data;

        value.template Deserialize<Protocols>(data);
        _output.Write(data);
    }

    template <typename Reader, typename T>
    typename boost::disable_if<is_basic_type<T> >::type
    Write(const value<T, Reader>& value) const
    {
        Apply<Protocols>(SerializeTo<Protocols>(_output), value);
    }

protected:
    writer_type& _output;
    mutable uint32_t _level;
};


} // namespace bond
