
#include "attributes_reflection.h"
#include <bond/core/exception.h>

namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace Enum
    {
        const
        std::map<std::string, enum Enum> _name_to_value_Enum =
            boost::assign::map_list_of<std::string, enum Enum>
                ("Value1", Value1);

        const
        std::map<enum Enum, std::string> _value_to_name_Enum =
            bond::reverse_map(_name_to_value_Enum);

        const std::string& ToString(enum Enum value)
        {
            std::map<enum Enum, std::string>::const_iterator it =
                GetValueToNameMap(value).find(value);

            if (GetValueToNameMap(value).end() == it)
                bond::InvalidEnumValueException(value, "Enum");

            return it->second;
        }

        void FromString(const std::string& name, enum Enum& value)
        {
            std::map<std::string, enum Enum>::const_iterator it =
                _name_to_value_Enum.find(name);

            if (_name_to_value_Enum.end() == it)
                bond::InvalidEnumValueException(name.c_str(), "Enum");

            value = it->second;
        }

    } // namespace Enum
    } // namespace _bond_enumerators

    
    const bond::Metadata Foo::Schema::metadata
        = Foo::Schema::GetMetadata();
    
    const bond::Metadata Foo::Schema::s_f_metadata
        = bond::reflection::MetadataInit("f", bond::reflection::optional_field_modifier::value,
            boost::assign::map_list_of<std::string, std::string>
                    ("FieldAttribute1", "one")
                    ("FieldAttribute2", "two"));

    
} // namespace tests
