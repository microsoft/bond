
#pragma once

#include <stdint.h>

namespace tests
{
namespace _bond_enumerators
{
    
    namespace EnumType1
    {
        enum EnumType1
        {
            EnumValue1 = static_cast<int32_t>(5),
            EnumValue2 = static_cast<int32_t>(10),
            EnumValue3 = static_cast<int32_t>(-10),
            EnumValue4 = static_cast<int32_t>(42),
            Low = static_cast<int32_t>(1),
            EnumValue5 = static_cast<int32_t>(-10),
            EnumValue6 = static_cast<int32_t>(4294967286),
            Int32Min = static_cast<int32_t>(-2147483647-1),
            Int32Max = static_cast<int32_t>(2147483647),
            UInt32Min = static_cast<int32_t>(0),
            UInt32Max = static_cast<int32_t>(4294967295)
        };
    } // namespace EnumType1

} // namespace _bond_enumerators

using namespace _bond_enumerators::EnumType1;
} // namespace tests
