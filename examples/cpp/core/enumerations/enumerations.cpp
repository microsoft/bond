#include <cassert>
#include <limits>
#include <stdint.h>

#include "enumerations_enum.h"

using namespace examples::enumerations;

int main()
{
    Color color;

    color = Color::Orange;
    color = Yellow;

    Fruit fruit;

    fruit = Apple;
    fruit = Fruit::Orange;

    assert(Limits::Int32Min == std::numeric_limits<int32_t>::min());
    assert(Limits::Int32Max == std::numeric_limits<int32_t>::max());
    assert(Limits::UInt32Min == std::numeric_limits<uint32_t>::min());
    assert(Limits::UInt32Max == std::numeric_limits<uint32_t>::max());

    return 0;
}
