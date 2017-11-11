
#include "aliases_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace EnumToWrap
    {
        const
        std::map<std::string, enum EnumToWrap> _name_to_value_EnumToWrap
            {
                { "anEnumValue", anEnumValue }
            };

        const
        std::map<enum EnumToWrap, std::string> _value_to_name_EnumToWrap =
            ::bond::reverse_map(_name_to_value_EnumToWrap);

        const std::string& ToString(enum EnumToWrap value)
        {
            std::map<enum EnumToWrap, std::string>::const_iterator it =
                GetValueToNameMap(value).find(value);

            if (GetValueToNameMap(value).end() == it)
                ::bond::InvalidEnumValueException(value, "EnumToWrap");

            return it->second;
        }

        void FromString(const std::string& name, enum EnumToWrap& value)
        {
            if (!ToEnum(value, name))
                ::bond::InvalidEnumValueException(name.c_str(), "EnumToWrap");
        }
    } // namespace EnumToWrap
    } // namespace _bond_enumerators

    
    const ::bond::Metadata WrappingAnEnum::Schema::metadata
        = WrappingAnEnum::Schema::GetMetadata();
    
    const ::bond::Metadata WrappingAnEnum::Schema::s_aWrappedEnum_metadata
        = ::bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumToWrap::anEnumValue, "aWrappedEnum");

    
} // namespace tests
