
#include "attributes_reflection.h"
#include <bond/core/exception.h>
#include <unordered_map>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace Enum
    {
        namespace
        {
            struct _hash_Enum
            {
                std::size_t operator()(enum Enum value) const
                {
                    return static_cast<std::size_t>(value);
                }
            };
        }
        const std::string& ToString(enum Enum value)
        {
            const auto& map = GetValueToNameMap<std::unordered_map<enum Enum, std::string, _hash_Enum> >(value);
            auto it = map.find(value);

            if (map.end() == it)
                ::bond::InvalidEnumValueException(value, "Enum");

            return it->second;
        }

        void FromString(const std::string& name, enum Enum& value)
        {
            if (!ToEnum(value, name))
                ::bond::InvalidEnumValueException(name.c_str(), "Enum");
        }

        bool ToEnum(enum Enum& value, const std::string& name)
        {
            const auto& map = GetNameToValueMap<std::unordered_map<std::string, enum Enum> >(value);
            auto it = map.find(name);

            if (map.end() == it)
                return false;

            value = it->second;

            return true;
        }

        bool FromEnum(std::string& name, enum Enum value)
        {
            const auto& map = GetValueToNameMap<std::unordered_map<enum Enum, std::string, _hash_Enum> >(value);
            auto it = map.find(value);

            if (map.end() == it)
                return false;

            name = it->second;

            return true;
        }

    } // namespace Enum
    } // namespace _bond_enumerators

    
    const ::bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const ::bond::Metadata Foo::Schema::s_f_metadata
        = ::bond::reflection::MetadataInit("f", ::bond::reflection::optional_field_modifier::value,
                {
                    { "FieldAttribute1", "one" },
                    { "FieldAttribute2", "two" }
                });

    
} // namespace tests
