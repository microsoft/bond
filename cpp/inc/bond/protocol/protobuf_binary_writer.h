// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/protobuf_utils.h"
#include "detail/simple_array.h"

#include <bond/stream/output_counter.h>

#include <boost/locale.hpp>

/*
    Implements Protocol Buffers binary encoding.
    See https://developers.google.com/protocol-buffers/docs/encoding for details.
*/

namespace bond
{
    template <typename BufferT>
    class ProtobufBinaryWriter;

    template <typename BufferT>
    class ProtobufBinaryReader;


    class ProtobufBinaryCounter
    {
        template <typename Buffer>
        friend class ProtobufBinaryWriter;

    private:
        struct type : OutputCounter // Must be a new type and not an alias.
        {};
    };


    template <typename BufferT>
    class ProtobufBinaryWriter
    {
        class Pass1
        {
        public:
            Pass1(ProtobufBinaryWriter* writer)
                : _writer(writer)
            {}

            ~Pass1()
            {
                _writer->_it = nullptr;
            }

        private:
            ProtobufBinaryWriter* _writer;
        };

        using Counter = ProtobufBinaryCounter::type;

    public:
        using Buffer = BufferT;
        using Reader = ProtobufBinaryReader<Buffer>;
        using Pass0 = ProtobufBinaryWriter<Counter>;

        explicit ProtobufBinaryWriter(Buffer& output)
            : _output{ output },
              _it{ nullptr }
        {}

        template <typename T>
        ProtobufBinaryWriter(Counter& output, const ProtobufBinaryWriter<T>& /*pass1*/)
            : _output{ output },
              _it{ nullptr }
        {}

        bool NeedPass0() const
        {
            return _it == nullptr;
        }

        Pass1 WithPass0(Pass0& pass0)
        {
            _it = pass0._lengths.begin();
            return this;
        }

        void WriteVersion()
        {}

        //
        // Write methods
        //
        void WriteStructBegin(const Metadata& /*metadata*/, bool base)
        {
            if (base)
            {
                detail::proto::NotSupportedException("Inheritance");
            }

            if (!_fields.empty())
            {
                const auto& info = _fields.top(std::nothrow);

                if (info.level > 1)
                {
                    detail::proto::NotSupportedException("Container nesting");
                }

                WriteTag(info.has_element
                    ? info.element.value.tag
                    : detail::proto::MakeTag(info.field.id, WireType::LengthDelimited));
            }

            StructLengthBegin();
        }

        void WriteStructEnd(bool base = false)
        {
            BOOST_VERIFY(!base);

            LengthEnd();

            if (!_fields.empty())
            {
                auto& info = _fields.top(std::nothrow);

                if (info.has_element && info.element.map_tag != 0)
                {
                    KeyValueEnd(info);
                }
            }
        }

        template <typename T>
        void WriteField(uint16_t id, const Metadata& metadata, const T& value)
        {
            FieldInfo::Element elem;
            elem.encoding = detail::proto::ReadEncoding(get_type_id<T>::value, metadata);
            elem.tag = detail::proto::MakeTag(
                id,
                detail::proto::GetWireType(get_type_id<T>::value, elem.encoding));

            WriteTag(elem.tag);
            WriteScalar(value, elem);
        }

        void WriteFieldBegin(BondDataType type, uint16_t id, const Metadata& metadata)
        {
            FieldInfo info;
            info.has_element = false;
            info.level = 0;
            info.field.id = id;
            info.field.metadata = &metadata;

            _fields.push(info);
        }

        void WriteFieldBegin(BondDataType type, uint16_t id)
        {
            WriteFieldBegin(type, id, schema<Unknown>::type::metadata);
        }

        void WriteFieldEnd()
        {
            _fields.pop(std::nothrow);
        }

        void WriteContainerBegin(uint32_t size, BondDataType type)
        {
            auto& info = _fields.top(std::nothrow);
            ++info.level;

            BOOST_VERIFY(size != 0
                || (info.has_element && type == BT_INT8));  // Nested empty blob

            if (!info.has_element)
            {
                BOOST_ASSERT(info.field.metadata);

                FieldInfo::Element elem;
                elem.encoding = detail::proto::ReadEncoding(type, *info.field.metadata);

                switch (detail::proto::ReadPacking(type, *info.field.metadata))
                {
                case Packing::False:
                    elem.tag = detail::proto::MakeTag(
                        info.field.id,
                        type == BT_LIST // blob
                            ? WireType::LengthDelimited
                            : detail::proto::GetWireType(type, elem.encoding));
                    break;

                default:
                    WriteTag(detail::proto::MakeTag(info.field.id, WireType::LengthDelimited));
                    LengthBegin();
                    elem.tag = 0;
                    break;
                }

                info.element.map_tag = 0;
                info.element.value = elem;
                info.has_element = true;
            }
        }

        // container of 2-tuples (e.g. map)
        void WriteContainerBegin(uint32_t size, const std::pair<BondDataType, BondDataType>& type)
        {
            auto& info = _fields.top(std::nothrow);
            ++info.level;

            BOOST_VERIFY(size != 0);

            if (!info.has_element)
            {
                BOOST_ASSERT(info.field.metadata);

                const auto& metadata = *info.field.metadata;

                info.element.map_tag = detail::proto::MakeTag(info.field.id, WireType::LengthDelimited);

                info.element.key.encoding = detail::proto::ReadKeyEncoding(type.first, metadata);
                info.element.key.tag = detail::proto::MakeTag(
                    1,
                    detail::proto::GetWireType(type.first, info.element.key.encoding));

                info.element.value.encoding = detail::proto::ReadValueEncoding(type.second, metadata);
                info.element.value.tag = detail::proto::MakeTag(
                    2,
                    type.second == BT_LIST // blob
                        ? WireType::LengthDelimited
                        : detail::proto::GetWireType(type.second, info.element.value.encoding));

                info.element.is_key = true;
                info.has_element = true;
            }
        }

        // Write for container element
        template <typename T>
        typename boost::enable_if_c<is_basic_type<T>::value || std::is_same<T, blob>::value>::type
        Write(const T& value)
        {
            auto& info = _fields.top(std::nothrow);
            BOOST_ASSERT(info.has_element);

            if (info.level > (std::is_same<T, blob>::value ? 2 : 1))
            {
                detail::proto::NotSupportedException("Container nesting");
            }

            if (info.element.map_tag == 0)
            {
                WriteTaggedScalar(value, info.element.value);
            }
            else
            {
                WriteTaggedScalar(value, KeyValueBegin(info));
                KeyValueEnd(info);
            }
        }

        void WriteContainerEnd()
        {
            auto& info = _fields.top(std::nothrow);
            BOOST_ASSERT(info.has_element);

            --info.level;

            if (info.element.value.tag == 0)
            {
                LengthEnd();
            }
        }

    private:
        using FieldInfo = detail::proto::FieldInfo;
        using WireType = detail::proto::WireType;
        using Encoding = detail::proto::Encoding;
        using Packing = detail::proto::Packing;

        template <typename Buffer>
        friend class ProtobufBinaryWriter;

        template <typename T>
        void WriteVarInt(const T& value)
        {
            WriteVariableUnsigned(_output, value);
        }

        void WriteVarInt(const uint8_t& value)
        {
            WriteVarInt(static_cast<uint16_t>(value));
        }

        void WriteFixed(const uint32_t& value)
        {
            _output.Write(value);
        }

        void WriteFixed(const int32_t& value)
        {
            _output.Write(value);
        }

        void WriteFixed(const uint64_t& value)
        {
            _output.Write(value);
        }

        void WriteFixed(const int64_t& value)
        {
            _output.Write(value);
        }

        void WriteFixed(const float& value)
        {
            BOOST_STATIC_ASSERT(sizeof(float) == sizeof(uint32_t));
            _output.Write(value);
        }

        void WriteFixed(const double& value)
        {
            BOOST_STATIC_ASSERT(sizeof(double) == sizeof(uint64_t));
            _output.Write(value);
        }

        bool TryWriteTag(const FieldInfo::Element& elem)
        {
            if (elem.tag != 0)
            {
                WriteTag(elem.tag);
                return true;
            }

            return false;
        }

        void WriteTag(uint32_t tag)
        {
            BOOST_ASSERT(tag != 0);
            WriteVarInt(tag);
        }

        template <typename T>
        void WriteTaggedScalar(const T& value, const FieldInfo::Element& elem)
        {
            TryWriteTag(elem);
            WriteScalar(value, elem);
        }

        void WriteTaggedScalar(const blob& value, const FieldInfo::Element& elem)
        {
            if (elem.encoding != detail::proto::Unavailable<Encoding>())
            {
                detail::proto::NotSupportedException("Blob with encoding attribute");
            }

            const bool has_tag = TryWriteTag(elem);

            if (has_tag)
            {
                if (static_cast<WireType>(elem.tag & 0x7) != WireType::LengthDelimited)
                {
                    detail::proto::NotSupportedException("Blob with unpacked attribute");
                }

                LengthBegin();
            }

            _output.Write(value);

            if (has_tag)
            {
                LengthEnd();
            }
        }

        const FieldInfo::Element& KeyValueBegin(const FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);
            BOOST_ASSERT(info.element.map_tag != 0);

            if (info.element.is_key)
            {
                WriteTag(info.element.map_tag);
                LengthBegin();

                return info.element.key;
            }
            else
            {
                return info.element.value;
            }
        }

        void KeyValueEnd(FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);
            BOOST_ASSERT(info.element.map_tag != 0);

            if (info.element.is_key)
            {
                info.element.is_key = false;
            }
            else
            {
                LengthEnd();
                info.element.is_key = true;
            }
        }

        // Write for floating point
        template <typename T>
        typename boost::enable_if<std::is_floating_point<T> >::type
        WriteScalar(const T& value, const FieldInfo::Element& elem)
        {
            BOOST_STATIC_ASSERT(sizeof(T) <= sizeof(double));
            BOOST_VERIFY(elem.encoding == Encoding::Fixed);

            WriteFixed(value);
        }

        // Write for unsigned integers
        template <typename T>
        typename boost::enable_if<std::is_unsigned<T> >::type
        WriteScalar(const T& value, const FieldInfo::Element& elem)
        {
            BOOST_STATIC_ASSERT(sizeof(value) <= sizeof(uint64_t));

            switch (elem.encoding)
            {
            case Encoding::Fixed:
                WriteFixed(value);
                break;

            case Encoding::ZigZag:
                detail::proto::ZigZagEncodingException();
                break;

            default:
                WriteVarInt(value);
                break;
            }
        }

        // Write for signed integers
        template <typename T>
        typename boost::enable_if<is_signed_int<T> >::type
        WriteScalar(const T& value, const FieldInfo::Element& elem)
        {
            BOOST_STATIC_ASSERT(sizeof(value) <= sizeof(int64_t));

            switch (elem.encoding)
            {
            case Encoding::Fixed:
                WriteFixed(value);
                break;

            case Encoding::ZigZag:
                WriteVarInt(EncodeZigZag(value));
                break;

            default:
                WriteVarInt(static_cast<uint64_t>(value));
                break;
            }
        }

        // Write for enums
        template <typename T>
        typename boost::enable_if<std::is_enum<T> >::type
        WriteScalar(const T& value, const FieldInfo::Element& elem)
        {
            BOOST_STATIC_ASSERT(sizeof(value) == sizeof(int32_t));

            WriteScalar(static_cast<int32_t>(value), elem);
        }

        // Write for bool
        void WriteScalar(const bool& value, const FieldInfo::Element& elem)
        {
            BOOST_STATIC_ASSERT(sizeof(value) == sizeof(uint8_t));
            BOOST_VERIFY(elem.encoding == detail::proto::Unavailable<Encoding>());

            WriteVarInt(static_cast<uint8_t>(value));
        }

        // Write for strings
        template <typename T>
        typename boost::enable_if<is_string<T> >::type
        WriteScalar(const T& value, const FieldInfo::Element& elem)
        {
            BOOST_VERIFY(elem.tag != 0);
            BOOST_VERIFY(elem.encoding == detail::proto::Unavailable<Encoding>());

            LengthBegin();

            uint32_t length = string_length(value);
            detail::WriteStringData(_output, value, length);

            LengthEnd();
        }

        // Write for wstrings
        template <typename T>
        typename boost::enable_if<is_wstring<T> >::type
        WriteScalar(const T& value, const FieldInfo::Element& elem)
        {
            try
            {
                WriteScalar(boost::locale::conv::utf_to_utf<char>(value), elem);
            }
            catch (const boost::locale::conv::conversion_error&)
            {
                UnicodeConversionException();
            }
        }

        void LengthBegin()
        {
            LengthBegin(_output);
        }

        void StructLengthBegin()
        {
            StructLengthBegin(_output);
        }

        void LengthEnd()
        {
            LengthEnd(_output);
        }

        void LengthBegin(Counter& counter)
        {
            _stack.push(_lengths.size());
            _lengths.push(counter.GetCount());
        }

        void LengthEnd(Counter& counter)
        {
            uint32_t& length = _lengths[_stack.pop(std::nothrow)];

            length = counter.GetCount() - length;
            counter.WriteVariableUnsigned(length);
        }

        template <typename T>
        void LengthBegin(T&)
        {
            WriteVarInt(*_it++);
        }

        void StructLengthBegin(Counter& counter)
        {
            LengthBegin(counter);
        }

        template <typename T>
        void StructLengthBegin(T&)
        {
            if (!_fields.empty())
            {
                WriteVarInt(*_it);
            }

            ++_it;
        }

        template <typename T>
        void LengthEnd(T&)
        {}


        Buffer& _output;
        const uint32_t* _it;
        detail::SimpleArray<uint32_t> _stack;
        detail::SimpleArray<uint32_t> _lengths;
        detail::SimpleArray<FieldInfo> _fields;
    };


    template <typename Buffer> struct
    may_omit_fields<ProtobufBinaryWriter<Buffer> >
        : std::true_type {};

} // namespace bond
