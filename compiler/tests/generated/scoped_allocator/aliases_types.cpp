
#include "aliases_reflection.h"
#include <bond/core/exception.h>
#include <unordered_map>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace EnumToWrap
    {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
        const std::map<std::string, enum EnumToWrap> _name_to_value_EnumToWrap
            {
                { "anEnumValue", anEnumValue }
            };

        const std::map<enum EnumToWrap, std::string> _value_to_name_EnumToWrap
            {
                { anEnumValue, "anEnumValue" }
            };
#else
        namespace
        {
            struct _hash_EnumToWrap
            {
                std::size_t operator()(enum EnumToWrap value) const
                {
                    return static_cast<std::size_t>(value);
                }
            };
        }
#endif
        const std::string& ToString(enum EnumToWrap value)
        {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
            const auto& map = GetValueToNameMap(value);
#else
            const auto& map = GetValueToNameMap<std::unordered_map<enum EnumToWrap, std::string, _hash_EnumToWrap> >(value);
#endif
            auto it = map.find(value);

            if (map.end() == it)
                ::bond::InvalidEnumValueException(value, "EnumToWrap");

            return it->second;
        }

        void FromString(const std::string& name, enum EnumToWrap& value)
        {
            if (!ToEnum(value, name))
                ::bond::InvalidEnumValueException(name.c_str(), "EnumToWrap");
        }

        bool ToEnum(enum EnumToWrap& value, const std::string& name)
        {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
            const auto& map = GetNameToValueMap(value);
#else
            const auto& map = GetNameToValueMap<std::unordered_map<std::string, enum EnumToWrap> >(value);
#endif
            auto it = map.find(name);

            if (map.end() == it)
                return false;

            value = it->second;

            return true;
        }

        bool FromEnum(std::string& name, enum EnumToWrap value)
        {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
            const auto& map = GetValueToNameMap(value);
#else
            const auto& map = GetValueToNameMap<std::unordered_map<enum EnumToWrap, std::string, _hash_EnumToWrap> >(value);
#endif
            auto it = map.find(value);

            if (map.end() == it)
                return false;

            name = it->second;

            return true;
        }

    } // namespace EnumToWrap
    } // namespace _bond_enumerators

    
    const ::bond::Metadata WrappingAnEnum::Schema::metadata
        = WrappingAnEnum::Schema::GetMetadata();
    
    const ::bond::Metadata WrappingAnEnum::Schema::s_aWrappedEnum_metadata
        = ::bond::reflection::MetadataInit(::tests::_bond_enumerators::EnumToWrap::anEnumValue, "aWrappedEnum");

    
} // namespace tests
