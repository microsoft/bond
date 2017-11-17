
#include "attributes_reflection.h"
#include <bond/core/exception.h>
#include <unordered_map>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace Enum
    {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
        const std::map<std::string, enum Enum> _name_to_value_Enum
            {
                { "Value1", Value1 }
            };

        const std::map<enum Enum, std::string> _value_to_name_Enum
            {
                { Value1, "Value1" }
            };
#else
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
#endif
        const std::string& ToString(enum Enum value)
        {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
            const auto& map = GetValueToNameMap(value);
#else
            const auto& map = GetValueToNameMap<std::unordered_map<enum Enum, std::string, _hash_Enum> >(value);
#endif
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
#if defined(_MSC_VER) && (_MSC_VER < 1900)
            const auto& map = GetNameToValueMap(value);
#else
            const auto& map = GetNameToValueMap<std::unordered_map<std::string, enum Enum> >(value);
#endif
            auto it = map.find(name);

            if (map.end() == it)
                return false;

            value = it->second;

            return true;
        }

        bool FromEnum(std::string& name, enum Enum value)
        {
#if defined(_MSC_VER) && (_MSC_VER < 1900)
            const auto& map = GetValueToNameMap(value);
#else
            const auto& map = GetValueToNameMap<std::unordered_map<enum Enum, std::string, _hash_Enum> >(value);
#endif
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
