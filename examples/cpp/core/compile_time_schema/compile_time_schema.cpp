#include "compile_time_schema_reflection.h"

using namespace examples::compile_time_schema;

int main()
{
    // Get value of attribute "MyAttribute" of field Struct2::mapStruct1
    // (error checking omitted for brevity)
    std::string my_attribute = Struct2::Schema::var::mapStruct1::metadata.attributes.find("MyAttribute")->second;

    // Get default value of field Struct1::str
    std::string def = Struct1::Schema::var::str::metadata.default_value.string_value;

    // Declare variable of the same type as field Struct1::items
    Struct1::Schema::var::items::field_type x;

    // Get id of field Struct2::nullableStruct1
    uint16_t id = Struct2::Schema::var::nullableStruct1::id;

    BOOST_VERIFY(id == 1);

    // Assert at compile time that field Struct1::n is optional
    BOOST_STATIC_ASSERT((std::is_same<Struct1::Schema::var::n::field_modifier, 
                                      bond::reflection::optional_field_modifier>::value));

    return 0;    
}
