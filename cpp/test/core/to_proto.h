#pragma once

#include <bond/core/bond.h>
#include <bond/core/tuple.h>

#include <google/protobuf/message.h>


namespace bond
{
namespace detail
{
namespace proto
{
    template <typename T, typename Enable = void> struct
    is_blob_container
        : std::false_type {};

    template <typename T> struct
    is_blob_container<T, typename boost::enable_if<is_list_container<T> >::type>
        : std::is_same<typename element_type<T>::type, blob::value_type> {};

    template <typename T> struct
    is_blob
        : std::integral_constant<bool, is_blob_container<T>::value || std::is_same<T, blob>::value> {};


    class ToProto : public SerializingTransform
    {
    public:
        explicit ToProto(google::protobuf::Message& msg)
            : _message{ msg },
              _reflection{ *_message.GetReflection() },
              _descriptor{ *_message.GetDescriptor() }
        {}

        void Begin(const Metadata&) const
        {}

        void End() const
        {}

        void UnknownEnd() const
        {}

        template <typename T>
        bool Base(T&&) const
        {
            BOOST_ASSERT(false);
            return false;
        }

        template <typename T>
        typename boost::disable_if_c<is_container<T>::value && !is_blob<T>::value, bool>::type
        Field(uint16_t id, const Metadata& /*metadata*/, const T& value) const
        {
            SetValue(GetField(id), value);
            return false;
        }

        template <typename T>
        typename boost::enable_if_c<is_container<T>::value && !is_blob_container<T>::value, bool>::type
        Field(uint16_t id, const Metadata& /*metadata*/, const T& value) const
        {
            const auto& field = GetField(id);
            for (const_enumerator<T> items(value); items.more(); )
            {
                AddValue(field, items.next());
            }
            return false;
        }

        template <typename T>
        typename boost::disable_if_c<std::is_same<T, blob::value_type>::value, bool>::type
        Field(uint16_t id, const Metadata& /*metadata*/, const nullable<T>& value) const
        {
            if (value.hasvalue())
            {
                const auto& field = GetField(id);
                if (field.is_repeated())
                {
                    AddValue(field, value.value());
                }
                else
                {
                    SetValue(field, value.value());
                }
            }
            return false;
        }

        template <typename T, typename Reader>
        typename boost::enable_if<is_basic_type<T>, bool>::type
        Field(uint16_t id, const Metadata& metadata, const value<T, Reader>& value) const
        {
            T data{};
            value.Deserialize(data);
            return Field(id, metadata, data);
        }

        template <typename T>
        bool UnknownField(uint16_t /*id*/, const T& /*value*/) const
        {
            BOOST_ASSERT(false); // Not implemented
            return false;
        }

        template <typename T>
        void Container(const T& /*element*/, uint32_t /*size*/) const
        {
            BOOST_ASSERT(false); // Not implemented
        }

        template <typename Key, typename T>
        void Container(const Key& /*key*/, const T& /*value*/, uint32_t /*size*/) const
        {
            BOOST_ASSERT(false); // Not implemented
        }

    private:
        const google::protobuf::FieldDescriptor& GetField(uint16_t id) const
        {
            auto field = _descriptor.FindFieldByNumber(id);
            BOOST_ASSERT(field);
            return *field;
        }

        template <typename T1, typename T2>
        void AddValue(const google::protobuf::FieldDescriptor& field, const std::pair<T1, T2>& value) const
        {
            AddValue(field, std::forward_as_tuple(std::ignore, value.first, value.second));
        }

        template <typename T>
        typename boost::enable_if<is_bond_type<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            auto msg = _reflection.MutableMessage(&_message, &field);
            BOOST_ASSERT(msg);
            Apply(ToProto{ *msg }, value);
        }

        template <typename T, typename Reader>
        typename boost::disable_if<is_basic_type<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const value<T, Reader>& value) const
        {
            auto msg = _reflection.MutableMessage(&_message, &field);
            BOOST_ASSERT(msg);
            Apply(ToProto{ *msg }, value);
        }

        template <typename T>
        typename boost::enable_if<is_bond_type<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            auto msg = _reflection.AddMessage(&_message, &field);
            BOOST_ASSERT(msg);
            Apply(ToProto{ *msg }, value);
        }

        template <typename T>
        typename boost::enable_if<std::is_enum<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.SetEnumValue(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<std::is_enum<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.AddEnumValue(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<is_signed_int<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.SetInt32(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<is_signed_int<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.AddInt32(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<std::is_unsigned<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.SetUInt32(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<std::is_unsigned<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.AddUInt32(&_message, &field, value);
        }

        void SetValue(const google::protobuf::FieldDescriptor& field, const google::protobuf::string& value) const
        {
            _reflection.SetString(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<is_string<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            SetValue(field, google::protobuf::string{ string_data(value), string_length(value) });
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, const google::protobuf::string& value) const
        {
            _reflection.AddString(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<is_string<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            AddValue(field, google::protobuf::string{ string_data(value), string_length(value) });
        }

        template <typename T>
        typename boost::enable_if<is_wstring<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            SetValue(field, boost::locale::conv::utf_to_utf<char>(value));
        }

        template <typename T>
        typename boost::enable_if<is_wstring<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            AddValue(field, boost::locale::conv::utf_to_utf<char>(value));
        }

        void SetValue(const google::protobuf::FieldDescriptor& field, bool value) const
        {
            _reflection.SetBool(&_message, &field, value);
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, bool value) const
        {
            _reflection.AddBool(&_message, &field, value);
        }

        void SetValue(const google::protobuf::FieldDescriptor& field, float value) const
        {
            _reflection.SetFloat(&_message, &field, value);
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, float value) const
        {
            _reflection.AddFloat(&_message, &field, value);
        }

        void SetValue(const google::protobuf::FieldDescriptor& field, double value) const
        {
            _reflection.SetDouble(&_message, &field, value);
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, double value) const
        {
            _reflection.AddDouble(&_message, &field, value);
        }

        void SetValue(const google::protobuf::FieldDescriptor& field, int64_t value) const
        {
            _reflection.SetInt64(&_message, &field, value);
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, int64_t value) const
        {
            _reflection.AddInt64(&_message, &field, value);
        }

        void SetValue(const google::protobuf::FieldDescriptor& field, uint64_t value) const
        {
            _reflection.SetUInt64(&_message, &field, value);
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, uint64_t value) const
        {
            _reflection.AddUInt64(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<is_blob<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            SetValue(field, ToString(value));
        }

        template <typename T>
        typename boost::enable_if<is_blob<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            AddValue(field, ToString(value));
        }

        template <typename T>
        static google::protobuf::string ToString(const T& value)
        {
            google::protobuf::string str;
            for (const_enumerator<T> items(value); items.more(); )
            {
                str.push_back(static_cast<google::protobuf::string::value_type>(items.next()));
            }
            return str;
        }

        static google::protobuf::string ToString(const blob& value)
        {
            return google::protobuf::string{ value.content(), value.size() };
        }

        google::protobuf::Message& _message;
        const google::protobuf::Reflection& _reflection;
        const google::protobuf::Descriptor& _descriptor;
    };

} // namespace proto
} // namespace detail
} // namespace bond
