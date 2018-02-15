#include "precompiled.h"
#include "unit_test_util.h"
#include "to_proto.h"

#include "protobuf_writer_apply.h"
#include "protobuf_writer_reflection.h"
#include "protobuf_writer.pb.h"

#include <bond/protocol/protobuf_binary_writer.h>

#include <google/protobuf/wrappers.pb.h>
#include <google/protobuf/util/message_differencer.h>

#include <boost/mpl/joint_view.hpp>
#include <boost/test/debug.hpp>
#include <boost/test/unit_test.hpp>


BOOST_AUTO_TEST_SUITE(ProtobufWriterTests)

template <typename Bond, typename Proto>
void CheckBinaryFormat(
    const bond::bonded<Bond>& bond_struct,
    const Proto& proto_struct,
    const bond::blob& proto_data,
    bool require_same_size = true)
{
    bond::OutputBuffer output;
    bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);
    bond_struct.Serialize(writer);

    bond::blob bond_data = output.GetBuffer();

    if (require_same_size)
    {
        BOOST_REQUIRE_EQUAL(bond_data.size(), proto_data.size());
    }

    Proto proto_struct2;
    BOOST_CHECK(
        (bond_data == proto_data)
        || (proto_struct2.ParseFromString(google::protobuf::string{ bond_data.content(), bond_data.size() })
            && google::protobuf::util::MessageDifferencer::Equals(proto_struct, proto_struct2)));
}

template <typename Proto, typename Bond>
void CheckBinaryFormat(const Bond& bond_struct)
{
    Proto proto_struct;
    bond::Apply(bond::detail::proto::ToProto{ proto_struct }, bond_struct);

    auto size = proto_struct.ByteSizeLong();
    auto buffer = boost::make_shared_noinit<char[]>(size);
    BOOST_REQUIRE(proto_struct.SerializeToArray(buffer.get(), static_cast<int>(size)));
    bond::blob proto_data{ buffer, static_cast<uint32_t>(size) };

    // Serialization
    CheckBinaryFormat(bond::bonded<Bond>{ boost::ref(bond_struct) }, proto_struct, proto_data);

    // Transcoding from compact binary
    CheckBinaryFormat(
        GetBonded<
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer>,
            Bond>(bond_struct),
        proto_struct,
        proto_data);

    // Transcoding from simple binary
    CheckBinaryFormat(
        GetBonded<
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer>,
            Bond>(bond_struct),
        proto_struct,
        proto_data,
        false);     // Simple binary does not omit fields
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

    if (bond_struct != Bond{})
    {
        bond::OutputBuffer output;
        bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);
        BOOST_CHECK_THROW(bond::Serialize(bond_struct, writer), bond::CoreException);
    }
}

template <typename List, typename... T>
using expand = boost::mpl::joint_view<List, boost::mpl::list<T...> >;

using blob_types = boost::mpl::list<bond::blob, std::vector<int8_t>, bond::nullable<int8_t> >;
using string_types = boost::mpl::list<std::string, std::wstring>;
using unsigned_integer_types = boost::mpl::list<uint8_t, uint16_t, uint32_t, uint64_t>;
using integer_types = expand<unsigned_integer_types, int16_t, int32_t, int64_t, unittest::Enum>;
using basic_types = expand<integer_types, bool, float, double, std::string, std::wstring>;

BOOST_AUTO_TEST_CASE(InheritanceTests)
{
    CheckUnsupportedType<unittest::Derived>();
}

using FieldOrdinalTests_Types = boost::mpl::list<
    unittest::Field0, unittest::Field19000, unittest::Field19111, unittest::Field19999>;
BOOST_AUTO_TEST_CASE_TEMPLATE(FieldOrdinalTests, T, FieldOrdinalTests_Types)
{
    CheckUnsupportedType<T>();
}

BOOST_AUTO_TEST_CASE(IntegerTests)
{
    CheckBinaryFormat<unittest::proto::Integers, unittest::Integers>();
}

using IntegerWrongEncodingTests_Types = expand<integer_types, int8_t>;
BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerWrongEncodingTests, T, IntegerWrongEncodingTests_Types)
{
    CheckUnsupportedType<unittest::BoxWrongEncoding<T> >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerUnsignedZigZagTests, T, unsigned_integer_types)
{
    CheckUnsupportedType<unittest::BoxZigZag<T> >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(StringTests, T, string_types)
{
    CheckBinaryFormat<
        google::protobuf::StringValue, unittest::BoxWrongPackingWrongEncoding<T> >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(BlobTests, T, blob_types)
{
    CheckBinaryFormat<
        google::protobuf::BytesValue, unittest::BoxWrongPackingWrongEncoding<T> >();
}

BOOST_AUTO_TEST_CASE(IntegerContainerTests)
{
    CheckBinaryFormat<
        unittest::proto::IntegersContainer, unittest::IntegersContainer>();

    CheckBinaryFormat<
        unittest::proto::UnpackedIntegersContainer, unittest::UnpackedIntegersContainer>();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerContainerWrongEncodingTests, T, integer_types)
{
    CheckUnsupportedType<unittest::BoxWrongEncoding<vector<T> > >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerContainerUnsignedZigZagTests, T, unsigned_integer_types)
{
    CheckUnsupportedType<unittest::BoxZigZag<vector<T> > >();
}

using IntegerContainerWrongPackingTests_Types = expand<integer_types, bool, float, double>;
BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerContainerWrongPackingTests, T,
    IntegerContainerWrongPackingTests_Types)
{
    CheckUnsupportedType<unittest::BoxWrongPacking<vector<T> > >();
}

BOOST_AUTO_TEST_CASE(IntegerSetContainerTests)
{
    CheckBinaryFormat<
        unittest::proto::IntegersContainer, unittest::IntegersSetContainer>();

    CheckBinaryFormat<
        unittest::proto::UnpackedIntegersContainer, unittest::UnpackedIntegersSetContainer>();
}

using IntegerSetContainerWrongEncodingTests_Types = expand<integer_types, int8_t>;
BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerSetContainerWrongEncodingTests, T,
    IntegerSetContainerWrongEncodingTests_Types)
{
    CheckUnsupportedType<unittest::BoxWrongEncoding<set<T> > >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerSetContainerUnsignedZigZagTests, T, unsigned_integer_types)
{
    CheckUnsupportedType<unittest::BoxZigZag<set<T> > >();
}

using IntegerSetContainerWrongPackingTests_Types =
    expand<integer_types, int8_t, bool, float, double>;
BOOST_AUTO_TEST_CASE_TEMPLATE(IntegerSetContainerWrongPackingTests, T,
    IntegerSetContainerWrongPackingTests_Types)
{
    CheckUnsupportedType<unittest::BoxWrongPacking<set<T> > >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(StringContainerTests, T, string_types)
{
    CheckBinaryFormat<
        unittest::proto::StringContainer,
        unittest::BoxWrongPackingWrongEncoding<std::vector<T> > >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(StringSetContainerTests, T, string_types)
{
    CheckBinaryFormat<
        unittest::proto::StringContainer,
        unittest::BoxWrongPackingWrongEncoding<std::set<T> > >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(BlobContainerTests, T, blob_types)
{
    CheckBinaryFormat<
        unittest::proto::BlobContainer,
        unittest::BoxWrongPackingWrongEncoding<std::vector<T> > >();

    CheckBinaryFormat<
        unittest::proto::BlobContainer,
        unittest::BoxWrongPackingWrongEncoding<bond::nullable<T> > >();

    unittest::BoxWrongPackingWrongEncoding<std::vector<T> > box;
    box.value.resize(2);
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

BOOST_AUTO_TEST_CASE_TEMPLATE(StringMapKeyTests, T, string_types)
{
    CheckBinaryFormat<
        unittest::proto::StringMapKey,
        unittest::BoxWrongPackingWrongKeyEncoding<std::map<T, uint32_t> > >();
}

BOOST_AUTO_TEST_CASE(IntegerMapValueTests)
{
    CheckBinaryFormat<unittest::proto::IntegerMapValues, unittest::IntegerMapValues>();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(StringMapValueTests, T, string_types)
{
    CheckBinaryFormat<
        unittest::proto::StringMapValue,
        unittest::BoxWrongPackingWrongValueEncoding<std::map<uint32_t, T> > >();
}

BOOST_AUTO_TEST_CASE_TEMPLATE(BlobMapValueTests, T, blob_types)
{
    CheckBinaryFormat<
        unittest::proto::BlobMapValue,
        unittest::BoxWrongPackingWrongValueEncoding<std::map<uint32_t, T> > >();
}

BOOST_AUTO_TEST_CASE(StructMapValueTests)
{
    CheckBinaryFormat<
        unittest::proto::StructMapValue,
        unittest::BoxWrongPackingWrongValueEncoding<std::map<uint32_t, unittest::Integers> > >();
}

using NestedVectorVectorTests_Types = expand<basic_types, bond::blob, unittest::Integers>;
BOOST_AUTO_TEST_CASE_TEMPLATE(NestedVectorVectorTests, T, NestedVectorVectorTests_Types)
{
    CheckUnsupportedType<unittest::Box<std::vector<std::vector<T> > > >();
}

using NestedVectorSetTests_Types = expand<basic_types, int8_t>;
BOOST_AUTO_TEST_CASE_TEMPLATE(NestedVectorSetTests, T, NestedVectorSetTests_Types)
{
    CheckUnsupportedType<unittest::Box<std::vector<std::set<T> > > >();
}

using NestedVectorMapTests_Types = expand<basic_types, int8_t, bond::blob, unittest::Integers>;
BOOST_AUTO_TEST_CASE_TEMPLATE(NestedVectorMapTests, T, NestedVectorMapTests_Types)
{
    CheckUnsupportedType<unittest::Box<std::vector<std::map<uint32_t, T> > > >();
}

using NestedMapVectorTests_Types = expand<basic_types, bond::blob, unittest::Integers>;
BOOST_AUTO_TEST_CASE_TEMPLATE(NestedMapVectorTests, T, NestedMapVectorTests_Types)
{
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::vector<T> > > >();
}

using NestedMapSetTests_Types = expand<basic_types, int8_t>;
BOOST_AUTO_TEST_CASE_TEMPLATE(NestedMapSetTests, T, NestedMapSetTests_Types)
{
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::set<T> > > >();
}

using NestedMapMapTests_Types = expand<basic_types, int8_t, bond::blob, unittest::Integers>;
BOOST_AUTO_TEST_CASE_TEMPLATE(NestedMapMapTests, T, NestedMapMapTests_Types)
{
    CheckUnsupportedType<unittest::Box<std::map<uint32_t, std::map<uint32_t, T> > > >();
}

BOOST_AUTO_TEST_CASE(ComplexStructTests)
{
    CheckBinaryFormat<unittest::proto::ComplexStruct, unittest::ComplexStruct>();
}

BOOST_AUTO_TEST_CASE(UnknownFieldsTests)
{
    using Bond = unittest::ComplexStruct;
    using Proto = unittest::proto::ComplexStruct;

    using BondSkippedString = std::tuple<
        const bond::detail::ignore_t&,
        decltype(Bond::item),
        decltype(Bond::mitems),
        decltype(Bond::items),
        const bond::detail::ignore_t&>; // str

    auto bond_struct = InitRandom<Bond>();

    auto skipped_string_struct = GetBonded<
        bond::CompactBinaryReader<bond::InputBuffer>,
        bond::CompactBinaryWriter<bond::OutputBuffer>,
        BondSkippedString>(bond_struct);

    // Ok with unknown string
    {
        bond::OutputBuffer output;
        bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);
        skipped_string_struct.Serialize(writer);
        auto bond_data = output.GetBuffer();

        Proto proto_struct;
        BOOST_REQUIRE(proto_struct.ParseFromString(
            google::protobuf::string{ bond_data.content(), bond_data.size() }));
        BOOST_CHECK_EQUAL(proto_struct.str(), bond_struct.str);

        Proto proto_struct2;
        bond::Apply(bond::detail::proto::ToProto{ proto_struct2 }, bond_struct);
        BOOST_CHECK(google::protobuf::util::MessageDifferencer::Equals(proto_struct, proto_struct2));
    }

    using BondSkippedMap = std::tuple<
        const bond::detail::ignore_t&,
        decltype(Bond::item),
        const bond::detail::ignore_t&,  // mitems
        decltype(Bond::items),
        decltype(Bond::str)>;

    auto skipped_map_struct = GetBonded<
        bond::CompactBinaryReader<bond::InputBuffer>,
        bond::CompactBinaryWriter<bond::OutputBuffer>,
        BondSkippedMap>(bond_struct);

    // Throw when cannot skip
    {
        bond::OutputBuffer output;
        bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);
        BOOST_CHECK_THROW(skipped_map_struct.Serialize(writer), bond::CoreException);
    }

    // Skip unknown map
    {
        bond::OutputBuffer output;
        bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output, true);
        skipped_map_struct.Serialize(writer);
        auto bond_data = output.GetBuffer();

        Proto proto_struct;
        BOOST_REQUIRE(proto_struct.ParseFromString(
            google::protobuf::string{ bond_data.content(), bond_data.size() }));
        BOOST_CHECK_EQUAL(proto_struct.mitems_size(), 0);

        Proto proto_struct2;
        bond::Apply(bond::detail::proto::ToProto{ proto_struct2 }, bond_struct);
        proto_struct2.clear_mitems();
        BOOST_CHECK(google::protobuf::util::MessageDifferencer::Equals(proto_struct, proto_struct2));
    }
}

BOOST_AUTO_TEST_SUITE_END()

bool init_unit_test()
{
    boost::debug::detect_memory_leaks(false);
    bond::RandomProtocolReader::Seed(time(nullptr), time(nullptr) / 1000);
    return true;
}
