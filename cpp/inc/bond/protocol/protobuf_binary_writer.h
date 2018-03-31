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
    namespace detail
    {
    namespace proto
    {
        inline uint32_t MakeTag(uint16_t id, WireType wireType)
        {
            BOOST_ASSERT(wireType != Unavailable<WireType>::value);

            if (id == 0 || (id >= 19000 && id <= 19999))
            {
                NotSupportedException("Field ordinal with value 0 or in the range 19000-19999");
            }

            uint32_t tag = (uint32_t(id) << 3) | uint32_t(wireType);
            BOOST_ASSERT(tag != 0);
            return tag;
        }

        struct FieldInfo
        {
            struct Element
            {
                uint32_t tag;
                Encoding encoding;
            };

            union
            {
                struct
                {
                    const Metadata* metadata;
                    uint16_t id;

                } field;

                struct
                {
                    uint32_t map_tag;
                    Element value;
                    Element key;
                    bool is_blob;
                    bool is_key;

                } element;
            };

            bool has_element;
            bool is_list;
        };

    } // namespace proto
    } // namespace detail


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

        explicit ProtobufBinaryWriter(Buffer& output, bool skip_unknowns = false)
            : _output{ output },
              _skip_unknowns{ skip_unknowns },
              _it{ nullptr }
        {}

        template <typename T>
        ProtobufBinaryWriter(Counter& output, const ProtobufBinaryWriter<T>& pass1)
            : _output{ output },
              _skip_unknowns{ pass1._skip_unknowns },
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
            WriteBasic(id, &metadata, value);
        }

        void WriteFieldBegin(BondDataType type, uint16_t id, const Metadata& metadata)
        {
            _fields.push(MakeFieldInfo(type, id, &metadata));
        }

        bool WriteFieldBegin(BondDataType type, uint16_t id)
        {
            if (_skip_unknowns)
            {
                return false;
            }

            _fields.push(MakeFieldInfo(type, id, nullptr));
            return true;
        }

        void WriteFieldEnd()
        {
            _fields.pop(std::nothrow);
        }

        void WriteContainerBegin(uint32_t size, BondDataType type)
        {
            auto& info = _fields.top(std::nothrow);

            if (!info.has_element)
            {
                bool is_blob;
                if (type == BT_INT8)
                {
                    is_blob = info.is_list;
                }
                else
                {
                    info.is_list = (type == BT_LIST);
                    is_blob = false;
                }

                FieldInfo::Element elem;
                Packing packing;

                if (is_blob)
                {
                    elem.encoding = detail::proto::Unavailable<Encoding>::value;
                    packing = Packing::False;
                }
                else
                {
                    elem.encoding = detail::proto::ReadEncoding(type, info.field.metadata);
                    packing = detail::proto::ReadPacking(type, info.field.metadata);
                }

                switch (packing)
                {
                case Packing::False:
                    elem.tag = detail::proto::MakeTag(
                        info.field.id,
                        info.is_list // blob
                            ? WireType::LengthDelimited
                            : detail::proto::GetWireType(type, elem.encoding));
                    break;

                default:
                    if (size != 0)
                    {
                        WriteTag(detail::proto::MakeTag(info.field.id, WireType::LengthDelimited));
                        LengthBegin();
                        elem.tag = 0;
                    }
                    else
                    {
                        elem.tag = 1; // Avoid LengthEnd call in WriteContainerEnd
                    }
                    break;
                }

                info.element.map_tag = 0;
                info.element.value = elem;
                info.element.is_blob = is_blob;
                info.has_element = true;
            }
            else if (info.is_list && type == BT_INT8) // blob
            {
                BOOST_ASSERT(!info.element.is_blob);
                info.element.is_blob = true;
            }
            else
            {
                detail::proto::NotSupportedException("Container nesting");
            }

            if (info.element.is_blob)
            {
                BlobBegin(info);
            }
        }

        // container of 2-tuples (e.g. map)
        void WriteContainerBegin(uint32_t size, const std::pair<BondDataType, BondDataType>& type)
        {
            auto& info = _fields.top(std::nothrow);

            if (!info.has_element)
            {
                const Metadata* metadata = info.field.metadata;

                info.is_list = (type.second == BT_LIST);
                info.element.map_tag = detail::proto::MakeTag(info.field.id, WireType::LengthDelimited);

                info.element.key.encoding = detail::proto::ReadKeyEncoding(type.first, metadata);
                info.element.key.tag = detail::proto::MakeTag(
                    1,
                    detail::proto::GetWireType(type.first, info.element.key.encoding));

                info.element.value.encoding = detail::proto::ReadValueEncoding(type.second, metadata);
                info.element.value.tag = detail::proto::MakeTag(
                    2,
                    info.is_list // blob
                        ? WireType::LengthDelimited
                        : detail::proto::GetWireType(type.second, info.element.value.encoding));

                info.element.is_key = true;
                info.element.is_blob = false;
                info.has_element = true;
            }
            else
            {
                detail::proto::NotSupportedException("Container nesting");
            }
        }

        template <typename T>
        typename boost::enable_if<is_basic_type<T> >::type
        Write(const T& value)
        {
            auto& info = _fields.top(std::nothrow);

            if (info.has_element)
            {
                WriteElement(value, info);
            }
            else
            {
                WriteBasic(info.field.id, info.field.metadata, value);
            }
        }

        void Write(const blob& value)
        {
            WriteElement(value, _fields.top(std::nothrow));
        }

        void WriteContainerEnd()
        {
            auto& info = _fields.top(std::nothrow);
            BOOST_ASSERT(info.has_element);

            if (info.element.is_blob)
            {
                BlobEnd(info);
                info.element.is_blob = false;
            }

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

        bool TryWriteTag(uint32_t tag)
        {
            if (tag != 0)
            {
                WriteTag(tag);
                return true;
            }

            return false;
        }

        void WriteTag(uint32_t tag)
        {
            BOOST_ASSERT(tag != 0);
            WriteVarInt(tag);
        }

        static FieldInfo MakeFieldInfo(BondDataType type, uint16_t id, const Metadata* metadata)
        {
            FieldInfo info;
            info.field.id = id;
            info.field.metadata = metadata;
            info.has_element = false;
            info.is_list = (type == BT_LIST);

            return info;
        }

        template <typename T>
        void WriteBasic(uint16_t id, const Metadata* metadata, const T& value)
        {
            Encoding encoding = detail::proto::ReadEncoding(get_type_id<T>::value, metadata);

            WriteTag(detail::proto::MakeTag(id, detail::proto::GetWireType(get_type_id<T>::value, encoding)));
            WriteScalar(value, encoding);
        }

        template <typename T>
        void WriteElement(const T& value, FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);

            WriteTaggedElement(value, info);
        }

        void WriteElement(const blob::value_type& value, FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);

            if (info.element.is_blob)
            {
                _output.Write(value);
            }
            else
            {
                WriteTaggedElement(value, info);
            }
        }

        void WriteElement(const blob& value, FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);

            if (info.element.map_tag != 0)
            {
                KeyValueBegin(info);
            }

            _output.Write(value);
        }

        template <typename T>
        void WriteTaggedElement(const T& value, FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);

            const bool is_map = (info.element.map_tag != 0);

            const auto& elem = is_map ? KeyValueBegin(info) : info.element.value;
            TryWriteTag(elem.tag);
            WriteScalar(value, elem.encoding);

            if (is_map)
            {
                KeyValueEnd(info);
            }
        }

        void BlobBegin(const FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);
            BOOST_ASSERT(info.element.is_blob);

            if (info.element.map_tag != 0)
            {
                KeyValueBegin(info);
            }

            if (TryWriteTag(info.element.value.tag))
            {
                LengthBegin();
            }
        }

        void BlobEnd(FieldInfo& info)
        {
            BOOST_ASSERT(info.has_element);
            BOOST_ASSERT(info.element.is_blob);

            if (info.element.value.tag != 0)
            {
                LengthEnd();
            }

            if (info.element.map_tag != 0)
            {
                KeyValueEnd(info);
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
        WriteScalar(const T& value, Encoding encoding)
        {
            BOOST_STATIC_ASSERT(sizeof(T) <= sizeof(double));
            BOOST_VERIFY(encoding == Encoding::Fixed);

            WriteFixed(value);
        }

        // Write for unsigned integers
        template <typename T>
        typename boost::enable_if<std::is_unsigned<T> >::type
        WriteScalar(const T& value, Encoding encoding)
        {
            BOOST_STATIC_ASSERT(sizeof(value) <= sizeof(uint64_t));

            switch (encoding)
            {
            case Encoding::Fixed:
                WriteFixed(value);
                break;

            case Encoding::ZigZag:
                BOOST_ASSERT(false);
                break;

            default:
                WriteVarInt(value);
                break;
            }
        }

        // Write for signed integers
        template <typename T>
        typename boost::enable_if<is_signed_int<T> >::type
        WriteScalar(const T& value, Encoding encoding)
        {
            BOOST_STATIC_ASSERT(sizeof(value) <= sizeof(int64_t));

            switch (encoding)
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
        WriteScalar(const T& value, Encoding encoding)
        {
            BOOST_STATIC_ASSERT(sizeof(value) == sizeof(int32_t));

            WriteScalar(static_cast<int32_t>(value), encoding);
        }

        // Write for bool
        void WriteScalar(const bool& value, Encoding encoding)
        {
            BOOST_STATIC_ASSERT(sizeof(value) == sizeof(uint8_t));
            BOOST_VERIFY(encoding == detail::proto::Unavailable<Encoding>::value);

            WriteVarInt(static_cast<uint8_t>(value));
        }

        // Write for strings
        template <typename T>
        typename boost::enable_if<is_string<T> >::type
        WriteScalar(const T& value, Encoding encoding)
        {
            BOOST_VERIFY(encoding == detail::proto::Unavailable<Encoding>::value);

            LengthBegin();

            uint32_t length = string_length(value);
            detail::WriteStringData(_output, value, length);

            LengthEnd();
        }

        // Write for wstrings
        template <typename T>
        typename boost::enable_if<is_wstring<T> >::type
        WriteScalar(const T& value, Encoding encoding)
        {
            try
            {
                WriteScalar(boost::locale::conv::utf_to_utf<char>(value), encoding);
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
        const bool _skip_unknowns;
        const uint32_t* _it;
        detail::SimpleArray<uint32_t> _stack;
        detail::SimpleArray<uint32_t> _lengths;
        detail::SimpleArray<FieldInfo> _fields;
    };


    template <typename Buffer> struct
    may_omit_fields<ProtobufBinaryWriter<Buffer> >
        : std::true_type {};

} // namespace bond
