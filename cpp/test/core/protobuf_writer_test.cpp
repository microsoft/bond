#include "precompiled.h"
#include "unit_test_util.h"

#include "protobuf_writer_apply.h"
#include "protobuf_writer_reflection.h"
#include "protobuf_writer.pb.h"

#include <bond/core/tuple.h>
#include <bond/protocol/protobuf_binary_writer.h>

#include <google/protobuf/util/message_differencer.h>

#include <boost/test/debug.hpp>
#include <boost/test/unit_test.hpp>


namespace
{
    class ToProto : public bond::SerializingTransform
    {
    public:
        explicit ToProto(google::protobuf::Message& msg)
            : _message{ msg },
              _reflection{ *_message.GetReflection() },
              _descriptor{ *_message.GetDescriptor() }
        {}

        void Begin(const bond::Metadata&) const
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
        typename boost::disable_if<bond::is_container<T>, bool>::type
        Field(uint16_t id, const bond::Metadata& /*metadata*/, const T& value) const
        {
            SetValue(GetField(id), value);
            return false;
        }

        template <typename T>
        typename boost::enable_if<bond::is_container<T>, bool>::type
        Field(uint16_t id, const bond::Metadata& /*metadata*/, const T& value) const
        {
            const auto& field = GetField(id);
            for (bond::const_enumerator<T> items(value); items.more(); )
            {
                AddValue(field, items.next());
            }
            return false;
        }

        template <typename T, typename Reader>
        typename boost::enable_if<bond::is_basic_type<T>, bool>::type
        Field(uint16_t id, const bond::Metadata& metadata, const bond::value<T, Reader>& value) const
        {
            T data{};
            value.Deserialize(data);
            return Field(id, metadata, data);
        }

        template <typename T>
        bool Field(uint16_t id, const bond::Metadata& /*metadata*/, const bond::nullable<T>& value) const
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

        bool Field(uint16_t id, const bond::Metadata& /*metadata*/, const bond::blob& value) const
        {
            SetValue(GetField(id), value);
            return false;
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
        typename boost::enable_if<bond::is_bond_type<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            auto msg = _reflection.MutableMessage(&_message, &field);
            BOOST_ASSERT(msg);
            bond::Apply(ToProto{ *msg }, value);
        }

        template <typename T, typename Reader>
        typename boost::disable_if<bond::is_basic_type<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const bond::value<T, Reader>& value) const
        {
            auto msg = _reflection.MutableMessage(&_message, &field);
            BOOST_ASSERT(msg);
            bond::Apply(ToProto{ *msg }, value);
        }

        template <typename T>
        typename boost::enable_if<bond::is_bond_type<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            auto msg = _reflection.AddMessage(&_message, &field);
            BOOST_ASSERT(msg);
            bond::Apply(ToProto{ *msg }, value);
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
        typename boost::enable_if<bond::is_signed_int<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, T value) const
        {
            _reflection.SetInt32(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<bond::is_signed_int<T> >::type
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
        typename boost::enable_if<bond::is_string<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            SetValue(field, google::protobuf::string{ string_data(value), string_length(value) });
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, const google::protobuf::string& value) const
        {
            _reflection.AddString(&_message, &field, value);
        }

        template <typename T>
        typename boost::enable_if<bond::is_string<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& field, const T& value) const
        {
            AddValue(field, google::protobuf::string{ string_data(value), string_length(value) });
        }

        template <typename T>
        typename boost::enable_if<bond::is_wstring<T> >::type
        SetValue(const google::protobuf::FieldDescriptor& /*field*/, const T& /*value*/) const
        {
            BOOST_ASSERT(false);
        }

        template <typename T>
        typename boost::enable_if<bond::is_wstring<T> >::type
        AddValue(const google::protobuf::FieldDescriptor& /*field*/, const T& /*value*/) const
        {
            BOOST_ASSERT(false);
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

        void SetValue(const google::protobuf::FieldDescriptor& field, const bond::blob& value) const
        {
            SetValue(field, google::protobuf::string{ value.content(), value.size() });
        }

        void AddValue(const google::protobuf::FieldDescriptor& field, const bond::blob& value) const
        {
            AddValue(field, google::protobuf::string{ value.content(), value.size() });
        }

        google::protobuf::Message& _message;
        const google::protobuf::Reflection& _reflection;
        const google::protobuf::Descriptor& _descriptor;
    };

} // anonymous namespace

BOOST_AUTO_TEST_SUITE(ProtobufWriterTests)

template <typename Proto, typename Bond>
void CheckBinaryFormat(const Bond& bond_struct)
{
    bond::OutputBuffer output;
    bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);
    bond::Serialize(bond_struct, writer);

    Proto proto_struct;
    bond::Apply(ToProto{ proto_struct }, bond_struct);

    bond::blob bond_data = output.GetBuffer();
    BOOST_REQUIRE_EQUAL(bond_data.size(), proto_struct.ByteSizeLong());

    auto proto_data = boost::make_shared_noinit<char[]>(bond_data.size());
    BOOST_CHECK(proto_struct.SerializeToArray(proto_data.get(), bond_data.size()));

    Proto proto_struct2;
    BOOST_CHECK(
        (bond_data == bond::blob{ proto_data, bond_data.size() })
        || (proto_struct2.ParseFromString(google::protobuf::string{ bond_data.content(), bond_data.size() })
            && google::protobuf::util::MessageDifferencer::Equals(proto_struct, proto_struct2)));
}

template <typename Proto, typename Bond>
void CheckBinaryFormat()
{
    CheckBinaryFormat<Proto>(Bond{});
    CheckBinaryFormat<Proto>(InitRandom<Bond>());
}

template <typename Bond>
void CheckUnsupportedType()
{
    auto bond_struct = InitRandom<Bond>();

    bond::OutputBuffer output;
    bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);

    BOOST_CHECK_THROW(bond::Serialize(bond_struct, writer), bond::CoreException);
}

BOOST_AUTO_TEST_CASE(InheritanceTests)
{
    CheckUnsupportedType<unittest::Derived>();
}

BOOST_AUTO_TEST_CASE(FieldOrdinalTests)
{
    CheckUnsupportedType<unittest::Field0>();
    CheckUnsupportedType<unittest::Field19000>();
    CheckUnsupportedType<unittest::Field19111>();
    CheckUnsupportedType<unittest::Field19999>();
}

BOOST_AUTO_TEST_CASE(IntegerTests)
{
    CheckBinaryFormat<unittest::proto::Integers, unittest::Integers>();

    CheckUnsupportedType<unittest::BoxWrongEncoding<uint8_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<uint16_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<uint32_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<uint64_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<int8_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<int16_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<int32_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<int64_t> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<unittest::Enum> >();

    CheckUnsupportedType<unittest::BoxZigZag<uint8_t> >();
    CheckUnsupportedType<unittest::BoxZigZag<uint16_t> >();
    CheckUnsupportedType<unittest::BoxZigZag<uint32_t> >();
    CheckUnsupportedType<unittest::BoxZigZag<uint64_t> >();
}

BOOST_AUTO_TEST_CASE(StringTests)
{
    CheckBinaryFormat<unittest::proto::String, unittest::Box<std::string> >();
    CheckBinaryFormat<unittest::proto::String, unittest::BoxWrongEncoding<std::string> >();

    CheckUnsupportedType<unittest::Box<std::wstring> >();
}

BOOST_AUTO_TEST_CASE(BlobTests)
{
    CheckBinaryFormat<unittest::proto::Blob, unittest::Box<bond::blob> >();

    CheckUnsupportedType<unittest::BoxZigZag<bond::blob> >();
    CheckUnsupportedType<unittest::BoxFixed<bond::blob> >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<bond::blob> >();

    CheckUnsupportedType<unittest::BoxUnpacked<bond::blob> >();
    CheckUnsupportedType<unittest::BoxWrongPacking<bond::blob> >();
}

BOOST_AUTO_TEST_CASE(IntegerContainerTests)
{
    CheckBinaryFormat<unittest::proto::IntegersContainer, unittest::IntegersContainer>();
    CheckBinaryFormat<unittest::proto::UnpackedIntegersContainer, unittest::UnpackedIntegersContainer>();

    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<uint8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<uint16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<uint32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<uint64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<int8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<int16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<int32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<int64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<unittest::Enum> > >();

    CheckUnsupportedType<unittest::BoxZigZag<vector<uint8_t> > >();
    CheckUnsupportedType<unittest::BoxZigZag<vector<uint16_t> > >();
    CheckUnsupportedType<unittest::BoxZigZag<vector<uint32_t> > >();
    CheckUnsupportedType<unittest::BoxZigZag<vector<uint64_t> > >();

    CheckUnsupportedType<unittest::BoxWrongPacking<vector<uint8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<uint16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<uint32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<uint64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<int8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<int16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<int32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<int64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<unittest::Enum> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<bool> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<float> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<double> > >();
}

BOOST_AUTO_TEST_CASE(IntegerSetContainerTests)
{
    CheckBinaryFormat<unittest::proto::IntegersContainer, unittest::IntegersSetContainer>();
    CheckBinaryFormat<unittest::proto::UnpackedIntegersContainer, unittest::UnpackedIntegersSetContainer>();

    CheckUnsupportedType<unittest::BoxWrongEncoding<set<uint8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<uint16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<uint32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<uint64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<int8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<int16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<int32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<int64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<unittest::Enum> > >();

    CheckUnsupportedType<unittest::BoxZigZag<set<uint8_t> > >();
    CheckUnsupportedType<unittest::BoxZigZag<set<uint16_t> > >();
    CheckUnsupportedType<unittest::BoxZigZag<set<uint32_t> > >();
    CheckUnsupportedType<unittest::BoxZigZag<set<uint64_t> > >();

    CheckUnsupportedType<unittest::BoxWrongPacking<set<uint8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<uint16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<uint32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<uint64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<int8_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<int16_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<int32_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<int64_t> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<unittest::Enum> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<bool> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<float> > >();
    CheckUnsupportedType<unittest::BoxWrongPacking<set<double> > >();
}

BOOST_AUTO_TEST_CASE(StringContainerTests)
{
    CheckBinaryFormat<
        unittest::proto::StringContainer,
        unittest::BoxWrongPackingWrongEncoding<std::vector<std::string> > >();
}

BOOST_AUTO_TEST_CASE(StringSetContainerTests)
{
    CheckBinaryFormat<
        unittest::proto::StringContainer,
        unittest::BoxWrongPackingWrongEncoding<std::set<std::string> > >();
}

BOOST_AUTO_TEST_CASE(BlobContainerTests)
{
    CheckBinaryFormat<
        unittest::proto::BlobContainer,
        unittest::BoxWrongPackingWrongEncoding<std::vector<bond::blob> > >();

    unittest::BoxWrongPackingWrongEncoding<std::vector<bond::blob> > box;
    box.value.resize(2, bond::blob{});
    CheckBinaryFormat<unittest::proto::BlobContainer>(box);
}

BOOST_AUTO_TEST_CASE(StructContainerTests)
{
    CheckBinaryFormat<
        unittest::proto::StructContainer,
        unittest::BoxWrongPackingWrongEncoding<std::vector<unittest::Integers> > >();
}

BOOST_AUTO_TEST_CASE(NestedStructTests)
{
    CheckBinaryFormat<
        unittest::proto::NestedStruct,
        unittest::BoxWrongPackingWrongEncoding<unittest::Integers> >();

    unittest::BoxWrongPackingWrongEncoding<bond::bonded<unittest::Integers> > box;
    box.value = bond::bonded<unittest::Integers>{ InitRandom<unittest::Integers>() };
    CheckBinaryFormat<unittest::proto::NestedStruct>(box);
}

BOOST_AUTO_TEST_CASE(NullableTests)
{
    CheckBinaryFormat<unittest::proto::Nullable, unittest::Nullable>();
}

BOOST_AUTO_TEST_CASE(IntegerMapKeyTests)
{
    CheckBinaryFormat<unittest::proto::IntegerMapKeys, unittest::IntegerMapKeys>();

    CheckUnsupportedType<unittest::Box<std::map<float, uint32_t> > >();
    CheckUnsupportedType<unittest::Box<std::map<double, uint32_t> > >();
}

BOOST_AUTO_TEST_CASE(StringMapKeyTests)
{
    CheckBinaryFormat<
        unittest::proto::StringMapKey,
        unittest::Box<std::map<std::string, uint32_t> > >();

    CheckUnsupportedType<unittest::Box<std::map<std::wstring, uint32_t> > >();
}

BOOST_AUTO_TEST_CASE(IntegerMapValueTests)
{
    CheckBinaryFormat<unittest::proto::IntegerMapValues, unittest::IntegerMapValues>();
}

BOOST_AUTO_TEST_CASE(StringMapValueTests)
{
    CheckBinaryFormat<
        unittest::proto::StringMapValue,
        unittest::Box<std::map<uint32_t, std::string> > >();

    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::wstring> > >();
}

BOOST_AUTO_TEST_CASE(BlobMapValueTests)
{
    CheckBinaryFormat<
        unittest::proto::BlobMapValue,
        unittest::BoxWrongPackingWrongEncoding<std::map<uint32_t, bond::blob> > >();
}

BOOST_AUTO_TEST_CASE(StructMapValueTests)
{
    CheckBinaryFormat<
        unittest::proto::StructMapValue,
        unittest::BoxWrongPackingWrongEncoding<std::map<uint32_t, unittest::Integers> > >();
}

BOOST_AUTO_TEST_CASE(NestedContainersTests)
{
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<uint8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<uint16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<uint32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<uint64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<int8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<int16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<int32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<int64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<unittest::Enum> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<bool> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<float> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<double> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<std::string> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<std::wstring> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<bond::blob> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<unittest::Integers> > > >();

    CheckUnsupportedType<unittest::Box<std::vector<std::set<uint8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<uint16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<uint32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<uint64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<int8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<int16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<int32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<int64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<unittest::Enum> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<bool> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<float> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<double> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<std::string> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::set<std::wstring> > > >();

    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<uint8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<uint16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<uint32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<uint64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<int8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<int16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<int32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<int64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<unittest::Enum> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<bool> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<float> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<double> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<std::string> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<std::wstring> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<bond::blob> > > >();
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<unittest::Integers> > > >();

    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, uint8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, uint16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, uint32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, uint64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, int8_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, int16_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, int32_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, int64_t> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, unittest::Enum> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, bool> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, float> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, double> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, std::string> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, std::wstring> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, bond::blob> > > >();
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, unittest::Integers> > > >();
}

BOOST_AUTO_TEST_CASE(ComplexStructTests)
{
    CheckBinaryFormat<unittest::proto::ComplexStruct, unittest::ComplexStruct>();
}

BOOST_AUTO_TEST_SUITE_END()

bool init_unit_test()
{
    boost::debug::detect_memory_leaks(false);
    return true;
}
