
#include "defaults_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace EnumType1
    {
        const
        std::map<std::string, enum EnumType1> _name_to_value_EnumType1 =
            boost::assign::map_list_of<std::string, enum EnumType1>
                ("EnumValue1", EnumValue1)
                ("EnumValue2", EnumValue2)
                ("EnumValue3", EnumValue3)
                ("EnumValue4", EnumValue4)
                ("Low", Low)
                ("EnumValue5", EnumValue5)
                ("EnumValue6", EnumValue6)
                ("Int32Min", Int32Min)
                ("Int32Max", Int32Max)
                ("UInt32Min", UInt32Min)
                ("UInt32Max", UInt32Max);

        const
        std::map<enum EnumType1, std::string> _value_to_name_EnumType1 =
            bond::reverse_map(_name_to_value_EnumType1);

        const std::string& ToString(enum EnumType1 value)
        {
            std::map<enum EnumType1, std::string>::const_iterator it =
                GetValueToNameMap(value).find(value);

            if (GetValueToNameMap(value).end() == it)
                bond::InvalidEnumValueException(value, "EnumType1");

            return it->second;
        }

        void FromString(const std::string& name, enum EnumType1& value)
        {
            std::map<std::string, enum EnumType1>::const_iterator it =
                _name_to_value_EnumType1.find(name);

            if (_name_to_value_EnumType1.end() == it)
                bond::InvalidEnumValueException(name.c_str(), "EnumType1");

            value = it->second;
        }

    } // namespace EnumType1
    } // namespace _bond_enumerators

    
    const bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const bond::Metadata Foo::Schema::s_m_bool_1_metadata
        = bond::reflection::MetadataInit(true, "m_bool_1");
    
    const bond::Metadata Foo::Schema::s_m_bool_2_metadata
        = bond::reflection::MetadataInit(false, "m_bool_2");
    
    const bond::Metadata Foo::Schema::s_m_bool_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_bool_3");
    
    const bond::Metadata Foo::Schema::s_m_str_1_metadata
        = bond::reflection::MetadataInit("default string value", "m_str_1");
    
    const bond::Metadata Foo::Schema::s_m_str_2_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_str_2");
    
    const bond::Metadata Foo::Schema::s_m_int8_4_metadata
        = bond::reflection::MetadataInit(static_cast<int8_t>(-127), "m_int8_4");
    
    const bond::Metadata Foo::Schema::s_m_int8_5_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_int8_5");
    
    const bond::Metadata Foo::Schema::s_m_int16_4_metadata
        = bond::reflection::MetadataInit(static_cast<int16_t>(-32767), "m_int16_4");
    
    const bond::Metadata Foo::Schema::s_m_int16_5_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_int16_5");
    
    const bond::Metadata Foo::Schema::s_m_int32_4_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_int32_4");
    
    const bond::Metadata Foo::Schema::s_m_int32_max_metadata
        = bond::reflection::MetadataInit(static_cast<int32_t>(2147483647), "m_int32_max");
    
    const bond::Metadata Foo::Schema::s_m_int64_4_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_int64_4");
    
    const bond::Metadata Foo::Schema::s_m_int64_max_metadata
        = bond::reflection::MetadataInit(static_cast<int64_t>(9223372036854775807LL), "m_int64_max");
    
    const bond::Metadata Foo::Schema::s_m_uint8_2_metadata
        = bond::reflection::MetadataInit(static_cast<uint8_t>(255), "m_uint8_2");
    
    const bond::Metadata Foo::Schema::s_m_uint8_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_uint8_3");
    
    const bond::Metadata Foo::Schema::s_m_uint16_2_metadata
        = bond::reflection::MetadataInit(static_cast<uint16_t>(65535), "m_uint16_2");
    
    const bond::Metadata Foo::Schema::s_m_uint16_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_uint16_3");
    
    const bond::Metadata Foo::Schema::s_m_uint32_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_uint32_3");
    
    const bond::Metadata Foo::Schema::s_m_uint32_max_metadata
        = bond::reflection::MetadataInit(static_cast<uint32_t>(4294967295), "m_uint32_max");
    
    const bond::Metadata Foo::Schema::s_m_uint64_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_uint64_3");
    
    const bond::Metadata Foo::Schema::s_m_uint64_max_metadata
        = bond::reflection::MetadataInit(static_cast<uint64_t>(18446744073709551615ULL), "m_uint64_max");
    
    const bond::Metadata Foo::Schema::s_m_double_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_double_3");
    
    const bond::Metadata Foo::Schema::s_m_double_4_metadata
        = bond::reflection::MetadataInit(static_cast<double>(-123.456789), "m_double_4");
    
    const bond::Metadata Foo::Schema::s_m_double_5_metadata
        = bond::reflection::MetadataInit(static_cast<double>(-0.0), "m_double_5");
    
    const bond::Metadata Foo::Schema::s_m_float_3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_float_3");
    
    const bond::Metadata Foo::Schema::s_m_float_4_metadata
        = bond::reflection::MetadataInit(static_cast<float>(2.71828183f), "m_float_4");
    
    const bond::Metadata Foo::Schema::s_m_float_7_metadata
        = bond::reflection::MetadataInit(static_cast<float>(0.0f), "m_float_7");
    
    const bond::Metadata Foo::Schema::s_m_enum1_metadata
        = bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumType1::EnumValue1, "m_enum1");
    
    const bond::Metadata Foo::Schema::s_m_enum2_metadata
        = bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumType1::EnumValue3, "m_enum2");
    
    const bond::Metadata Foo::Schema::s_m_enum3_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_enum3");
    
    const bond::Metadata Foo::Schema::s_m_enum_int32min_metadata
        = bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumType1::Int32Min, "m_enum_int32min");
    
    const bond::Metadata Foo::Schema::s_m_enum_int32max_metadata
        = bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumType1::Int32Max, "m_enum_int32max");
    
    const bond::Metadata Foo::Schema::s_m_enum_uint32_min_metadata
        = bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumType1::UInt32Min, "m_enum_uint32_min");
    
    const bond::Metadata Foo::Schema::s_m_enum_uint32_max_metadata
        = bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumType1::UInt32Max, "m_enum_uint32_max");
    
    const bond::Metadata Foo::Schema::s_m_wstr_1_metadata
        = bond::reflection::MetadataInit(L"default wstring value", "m_wstr_1");
    
    const bond::Metadata Foo::Schema::s_m_wstr_2_metadata
        = bond::reflection::MetadataInit(bond::nothing, "m_wstr_2");

    
} // namespace tests
