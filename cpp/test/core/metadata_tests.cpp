#include "precompiled.h"
#include "metadata_tests.h"

#include <boost/format.hpp>

class DefaultValueVerifier
    : public bond::SerializingTransform
{
public:
    void Begin(const bond::Metadata& /*metadata*/) const
    {}

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(const T& /*value*/) const
    {
        return false;
    }

    template <typename T>
    bool Field(uint16_t /*id*/, const bond::Metadata& metadata, const T& value) const
    {
        VerifyDefault(metadata.default_value, value);
        return false;
    }


    template <typename T, typename Reader>
    bool Field(uint16_t /*id*/, const bond::Metadata& metadata, const bond::value<T, Reader>& value) const
    {
        T x;

        value.Deserialize(x);

        VerifyDefault(metadata.default_value, x);
        return false;
    }


    template <typename Reader>
    bool Field(uint16_t /*id*/, const bond::Metadata& /*metadata*/, const bond::value<void, Reader>& /*value*/) const
    {
        return false;
    }


    template <typename Reader>
    bool Field(uint16_t /*id*/, const bond::Metadata& /*metadata*/, const bond::bonded<void, Reader>& /*value*/) const
    {
        return false;
    }


    template <typename T>
    bool UnknownField(uint16_t /*id*/, const T& /*value*/) const
    {
        return false;
    }

private:
    template <typename T>
    typename boost::enable_if_c<bond::is_signed_int<T>::value || std::is_enum<T>::value>::type
    VerifyDefault(const bond::Variant& variant, const T& value) const
    {
        UT_AssertAreEqual(static_cast<T>(variant.int_value), value);
    }

    template <typename T>
    typename boost::enable_if<std::is_unsigned<T> >::type
    VerifyDefault(const bond::Variant& variant, const T& value) const
    {
        UT_AssertAreEqual(static_cast<T>(variant.uint_value), value);
    }

    void VerifyDefault(const bond::Variant& variant, bool value) const
    {
        UT_AssertAreEqual(!!variant.uint_value, value);
    }

    void VerifyDefault(const bond::Variant& variant, const std::string& value) const
    {
        UT_AssertAreEqual(variant.string_value, value);
    }

    void VerifyDefault(const bond::Variant& variant, const std::wstring& value) const
    {
        UT_AssertAreEqual(variant.wstring_value.c_str(), value.c_str());
    }

    void VerifyDefault(const bond::Variant& variant, double value) const
    {
        UT_AssertAreEqual(variant.double_value, value);
    }
};



class MetadataVerifier
    : public bond::SerializingTransform
{
    const char* _name;

public:
    MetadataVerifier(const char* name)
        : _name(name)
    {}

    void Begin(const bond::Metadata& metadata) const
    {
        std::string name = boost::str(boost::format("%sWithMetadata") % _name);
        std::string qualified_name =
            boost::str(boost::format("unittest.%sWithMetadata") % _name);

        UT_AssertIsTrue(metadata.name == name);
        UT_AssertIsTrue(metadata.qualified_name == qualified_name);

        bond::Metadata  metadata_copy = metadata;
        UT_AssertIsTrue(metadata_copy.attributes["struct_name"] == name);
        if (metadata_copy.attributes.find("struct_address") != metadata_copy.attributes.end())
        {
            UT_AssertIsTrue(metadata_copy.attributes["struct_address"] == "0xDEADBEEF");
        }
    }

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(const T& value) const
    {
        Apply(MetadataVerifier("Base"), value);
        return false;
    }

    template <typename T>
    bool Field(uint16_t id, const bond::Metadata& metadata, const T& value) const
    {
        bond::Metadata  metadata_copy = metadata;

        std::string name = boost::str(boost::format("%s%u") % _name % id);

        UT_AssertIsTrue(metadata_copy.name == name);
        UT_AssertIsTrue(metadata_copy.name == metadata_copy.attributes["field_name"]);
        UT_AssertIsTrue(ToString(metadata_copy.modifier) == metadata_copy.attributes["field_modifier"]);

        return Nested(value);
    }

    template <typename T>
    bool UnknownField(uint16_t /*id*/, const T& /*value*/) const
    {
        return false;
    }

private:
    template <typename T>
    typename boost::enable_if<bond::is_bond_type<T>, bool>::type
    Nested(const T& value) const
    {
        return bond::Apply(MetadataVerifier("Nested"), value);
    }

    template <typename T>
    typename boost::disable_if<bond::is_bond_type<T>, bool>::type
    Nested(const T&) const
    {
        return false;
    }
};


TEST_CASE_BEGIN(CompileTimeMetadataTests)
{
    {
        StructWithMetadata value;

        bond::Apply(MetadataVerifier("Struct"), value);
    }

    {
        StructWithDefaults value;

        bond::Apply(DefaultValueVerifier(), value);

        UT_AssertAreEqual(value.m_uint32_min, 0u);
        UT_AssertAreEqual(value.m_uint32_max, 0xFFFFFFFF);
        UT_AssertAreEqual(value.m_int32_min, -2147483647-1);
        UT_AssertAreEqual(value.m_int32_max, 2147483647);

        UT_AssertAreEqual(value.m_uint64_min, 0u);
        UT_AssertAreEqual(value.m_uint64_max, 0xFFFFFFFFFFFFFFFF);
        UT_AssertAreEqual(value.m_int64_min, -9223372036854775807LL-1);
        UT_AssertAreEqual(value.m_int64_max, 9223372036854775807);
    }
}
TEST_CASE_END


TEST_CASE_BEGIN(RuntimeMetadataTests)
{
    {
        StructWithMetadata               value;
        bond::bonded<StructWithMetadata> bonded(Serialize<bond::SimpleBinaryReader<bond::InputBuffer>,
                                                          bond::SimpleBinaryWriter<bond::OutputBuffer> >(value));
        bond::bonded<void>               bonded_void(bonded);

        bond::Apply(MetadataVerifier("Struct"), bonded_void);
    }

    {
        StructWithDefaults               value;
        bond::bonded<StructWithDefaults> bonded(Serialize<bond::SimpleBinaryReader<bond::InputBuffer>,
                                                          bond::SimpleBinaryWriter<bond::OutputBuffer> >(value));
        bond::bonded<void>               bonded_void(bonded);

        bond::Apply(DefaultValueVerifier(), bonded_void);
    }

    {
        UT_AssertIsFalse(BondStruct<import2::ImportedEnum>::Schema::metadata.name ==
                         BondStruct<unittest::ImportedEnum>::Schema::metadata.name);

        bond::SchemaDef schema = bond::GetRuntimeSchema<GenericMetadata>().GetSchema();

        UT_AssertAreEqual(std::size_t(7), schema.structs.size());
    }
}
TEST_CASE_END

TEST_CASE_BEGIN(get_list_sub_type_id_ListAndNullable)
{
    {
        // we test on the type's Schema instead of SchemaDef because--for
        // now--the list sub type is not present in TypeDef.

        UT_AssertAreEqual(
            bond::ListSubType::NULLABLE_SUBTYPE,
            bond::get_list_sub_type_id<ListVsNullable::Schema::var::nullableInt::field_type>::value);

        UT_AssertAreEqual(
            bond::ListSubType::NO_SUBTYPE,
            bond::get_list_sub_type_id<ListVsNullable::Schema::var::vectorInt::field_type>::value);

        UT_AssertAreEqual(
            bond::ListSubType::NO_SUBTYPE,
            bond::get_list_sub_type_id<ListVsNullable::Schema::var::listInt::field_type>::value);

        UT_AssertAreEqual(
            bond::ListSubType::BLOB_SUBTYPE,
            bond::get_list_sub_type_id<ListVsNullable::Schema::var::blobData::field_type>::value);
    }
}
TEST_CASE_END

struct NoSubTypeAsserter
{
    template <typename Field>
    void operator()(const Field&)
    {
        UT_AssertAreEqual(bond::ListSubType::NO_SUBTYPE,
                          bond::get_list_sub_type_id<typename Field::field_type>::value);
    }
};

TEST_CASE_BEGIN(EnsureUnknownSeqIDLType)
{
    {
        // we test on the type's Schema instead of SchemaDef because--for
        // now--the list sub type is not present in TypeDef.
        boost::mpl::for_each<StructWithDefaults::Schema::fields>(NoSubTypeAsserter());
    }
}
TEST_CASE_END

void MetadataTest::Initialize()
{
    UnitTestSuite suite("Metadata tests");

    AddTestCase<TEST_ID(0x2201), CompileTimeMetadataTests>
        (suite, "Compile-time metadata test");

    AddTestCase<TEST_ID(0x2201), get_list_sub_type_id_ListAndNullable>
        (suite, "Differentiate Between List And Nullable");

    AddTestCase<TEST_ID(0x2201), EnsureUnknownSeqIDLType>
        (suite, "Ensure Unknown SeqIDLType");

    TEST_SIMPLE_PROTOCOL(
        AddTestCase<TEST_ID(0x2201), RuntimeMetadataTests>
            (suite, "Runtime metadata test");
    );
}


bool init_unit_test()
{
    MetadataTest::Initialize();
    return true;
}
