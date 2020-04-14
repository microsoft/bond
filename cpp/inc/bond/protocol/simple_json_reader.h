// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/rapidjson_helper.h"
#include "encoding.h"

#include "rapidjson/document.h"
#include "rapidjson/reader.h"

#include <boost/make_shared.hpp>
#include <boost/none.hpp>
#include <boost/optional/optional.hpp>

namespace bond
{

template <typename BufferT>
class SimpleJsonWriter;


/// @brief Reader for Simple JSON
template <typename BufferT>
class SimpleJsonReader
{
public:
    typedef BufferT                         Buffer;
    typedef DOMParser<SimpleJsonReader&>    Parser;
    typedef SimpleJsonWriter<Buffer>        Writer;
    typedef rapidjson::Value                Field;

    BOND_STATIC_CONSTEXPR uint16_t magic = SIMPLE_JSON_PROTOCOL;
    BOND_STATIC_CONSTEXPR uint16_t version = 0x0001;

    /// @brief Construct from input buffer/stream containing serialized data.
    SimpleJsonReader(const Buffer& input)
        : _value(nullptr),
          _document(boost::make_shared<rapidjson::Document>()),
          _streamHolder(input)
    { }

    /// @brief Create a "child" SimpleJsonReader to read \c value, which is
    /// known to be a member of the document in \c parent.
    ///
    /// @warning \c parent must remain alive for the lifetime of this child.
    SimpleJsonReader(SimpleJsonReader& parent, const Field& value)
        : _value(&value),
          _document(parent._document),
          _streamHolder(parent)
    {
        // Must have an already-parsed parent
        BOOST_ASSERT(parent._value);
    }

    bool ReadVersion()
    {
        return false;
    }

    void Parse()
    {
        // Don't need to reparse for nested fields
        if (!_value || _value == _document.get())
        {
            const unsigned parseFlags = rapidjson::kParseIterativeFlag | rapidjson::kParseStopWhenDoneFlag;

            _document->ParseStream<parseFlags>(_streamHolder.Get());

            // If there were any parse errors, an exception should have been
            // thrown, as we define RAPIDJSON_PARSE_ERROR
            BOOST_ASSERT(!_document->HasParseError());
            _value = _document.get();
        }
    }

    const Field* FindField(uint16_t id, const Metadata& metadata, BondDataType type)
    {
        // BT_INT32 may be an enum. This allows us to decode symbolic enum values
        // when parsing using runtime schema. The assumption is that runtime schema
        // matches JSON payload. If it doesn't, nothing horrible will happen, but
        // we might not indicate a required field missing for an int32 field if we
        // mistake a string member with matching name for it.
        return FindField(id, metadata, type, type == BT_INT32);
    }

    const Field* FindField(uint16_t id, const Metadata& metadata, BondDataType type, bool is_enum);


    template <typename T>
    void Read(T& var)
    {
        detail::Read(*GetValue(), var);
    }

    template <typename T>
    void ReadContainerBegin(uint32_t&, T&)
    {
        BOOST_ASSERT(false);
    }

    void ReadContainerEnd()
    {
        BOOST_ASSERT(false);
    }

    template <typename T>
    void Skip()
    { }

    template <typename T>
    void Skip(const T&)
    { }


    bool operator==(const SimpleJsonReader& rhs) const
    {
        return _value == rhs._value;
    }

    /// @brief Access to underlying buffer
    const Buffer& GetBuffer() const
    {
        return _streamHolder.Get().GetBuffer();
    }

    /// @brief Access to underlying buffer
    Buffer& GetBuffer()
    {
        return _streamHolder.Get().GetBuffer();
    }

private:
    rapidjson::Value::ConstMemberIterator MemberBegin() const
    {
        return GetValue()->IsObject() ? GetValue()->MemberBegin() : rapidjson::Value::ConstMemberIterator();
    }

    rapidjson::Value::ConstMemberIterator MemberEnd() const
    {
        return GetValue()->IsObject() ? GetValue()->MemberEnd() : rapidjson::Value::ConstMemberIterator();
    }

    rapidjson::Value::ConstValueIterator ArrayBegin() const
    {
        return GetValue()->IsArray() ? GetValue()->Begin() : rapidjson::Value::ConstValueIterator();
    }

    rapidjson::Value::ConstValueIterator ArrayEnd() const
    {
        return GetValue()->IsArray() ? GetValue()->End() : rapidjson::Value::ConstValueIterator();
    }

    uint32_t ArraySize() const
    {
        return GetValue()->IsArray() ? GetValue()->Size() : 0;
    }

    const rapidjson::Value* GetValue() const
    {
        BOOST_ASSERT(_value);
        return _value;
    }

    template <typename Input>
    friend struct base_input;

    template <typename Protocols, typename A, typename T, typename Buffer>
    friend void DeserializeContainer(std::vector<bool, A>&, const T&, SimpleJsonReader<Buffer>&);

    template <typename Protocols, typename T, typename Buffer>
    friend void DeserializeContainer(blob&, const T&, SimpleJsonReader<Buffer>&);

    template <typename Protocols, typename X, typename T, typename Buffer>
    friend typename boost::enable_if<is_list_container<X> >::type
    DeserializeContainer(X&, const T&, SimpleJsonReader<Buffer>&);

    template <typename Protocols, typename X, typename T, typename Buffer>
    friend typename boost::enable_if<is_set_container<X> >::type
    DeserializeContainer(X&, const T&, SimpleJsonReader<Buffer>&);

    template <typename Protocols, typename X, typename T, typename Buffer>
    friend typename boost::enable_if<is_map_container<X> >::type
    DeserializeMap(X&, BondDataType, const T&, SimpleJsonReader<Buffer>&);

    const rapidjson::Value* _value;
    boost::shared_ptr<rapidjson::Document> _document;

    /// @brief Holds either an input stream XOR a pointer to some parent
    /// StreamHolder.
    class StreamHolder
    {
    public:
        explicit StreamHolder(const Buffer& input)
            : _stream(),
              _parent()
        {
            // could use boost::in_place to construct this in the
            // initializer list when the minimum version of Boost is 1.63
            _stream.emplace(input);
            BOOST_ASSERT(IsParent());
        }

        explicit StreamHolder(SimpleJsonReader& parent)
            : _stream(),
              // Resolve the real parent
              _parent(&parent._streamHolder.GetParent())
        {
            BOOST_ASSERT(_parent->IsParent());
            BOOST_ASSERT(!IsParent());
        }

        // Intentionaly deep copy. Copies of SimpleJsonReader are expected
        // to be deep copies, even if they're made from children.
        StreamHolder(const StreamHolder& other)
            : _stream(other.Get()),
              _parent()
        {
            BOOST_ASSERT(IsParent());
        }

        StreamHolder& operator=(const StreamHolder& other)
        {
            _stream.emplace(other.Get());
            _parent = nullptr;
            BOOST_ASSERT(IsParent());

            return *this;
        }

        StreamHolder(StreamHolder&&) = default;
        StreamHolder& operator=(StreamHolder&&) = default;

        const detail::RapidJsonInputStream<Buffer>& Get() const
        {
            return IsParent() ? _stream.value() : _parent->Get();
        }

        detail::RapidJsonInputStream<Buffer>& Get()
        {
            return IsParent() ? _stream.value() : _parent->Get();
        }

    private:
        bool IsParent() const
        {
            const bool isParent = _parent == nullptr;
            BOOST_ASSERT(isParent == static_cast<bool>(_stream)); // parents must have a stream
            return isParent;
        }

        StreamHolder& GetParent()
        {
            return IsParent() ? *this : *_parent;
        }

        boost::optional<detail::RapidJsonInputStream<Buffer>> _stream;
        StreamHolder* _parent;
    } _streamHolder;
};


template <typename Buffer>
BOND_CONSTEXPR_OR_CONST uint16_t SimpleJsonReader<Buffer>::magic;

// Disable fast pass-through optimization for Simple JSON
template <typename Input, typename Output> struct
is_protocol_same<SimpleJsonReader<Input>, SimpleJsonWriter<Output> >
    : std::false_type {};


} // namespace bond
