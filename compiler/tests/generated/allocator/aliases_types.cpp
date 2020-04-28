
#include "aliases_reflection.h"
#include <bond/core/exception.h>
#include <unordered_map>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace EnumToWrap
    {
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
        const std::string& ToString(enum EnumToWrap value)
        {
            const auto& map = GetValueToNameMap<std::unordered_map<enum EnumToWrap, std::string, _hash_EnumToWrap> >(value);
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
            const auto& map = GetNameToValueMap<std::unordered_map<std::string, enum EnumToWrap> >(value);
            auto it = map.find(name);

            if (map.end() == it)
                return false;

            value = it->second;

            return true;
        }

        bool FromEnum(std::string& name, enum EnumToWrap value)
        {
            const auto& map = GetValueToNameMap<std::unordered_map<enum EnumToWrap, std::string, _hash_EnumToWrap> >(value);
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
