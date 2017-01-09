#include <cassert>
#include <limits>
#include <stdint.h>
#include <bond/core/exception.h>
#include <boost/core/ignore_unused.hpp>

#include "enumerations_enum.h"
#include "enumerations_types.h"

using namespace examples::enumerations;

static void DisambiguateEnumsWithSameName();
static void EnumValueLimits();
static void ConversionToFromEnum();
static void ConversionsToFromString();

int main()
{
    DisambiguateEnumsWithSameName();
    EnumValueLimits();
    ConversionToFromEnum();
    ConversionsToFromString();

    return 0;
}

// Bond provides a standard-compliant solution for scoped enumerations in
// C++ that overcomes the limitations of normal C++ enumeration types.
void DisambiguateEnumsWithSameName()
{
    // Both Color and Fruit have a value named Orange.

    Color color;

    color = Color::Orange;
    color = Yellow;
    boost::ignore_unused(color);
    assert(color == Yellow);

    Fruit fruit;

    fruit = Apple;
    fruit = Fruit::Orange;
    boost::ignore_unused(fruit);
    assert(fruit == Fruit::Orange);
}

// Bond enums are represented as signed 32-bit integers on the wire, but
// implicit conversions allow the comparison to uint32_t values.
void EnumValueLimits()
{
    assert(Limits::Int32Min == std::numeric_limits<int32_t>::min());
    assert(Limits::Int32Max == std::numeric_limits<int32_t>::max());
    assert(Limits::UInt32Min == std::numeric_limits<uint32_t>::min());
    assert(static_cast<uint32_t>(Limits::UInt32Max) == std::numeric_limits<uint32_t>::max());
}

// The ToEnum and FromEnum functions can be used to convert between enum
// values and their names. ToEnum and FromEnum return a bool indicating
// whether they were successful or not.
void ConversionToFromEnum()
{
    std::string name;
    bool result = FromEnum(name, Yellow);
    boost::ignore_unused(result);
    assert(result);
    assert(name == "Yellow");

    result = FromEnum(name, static_cast<Color>(100));
    assert(!result);

    Color value;
    result = ToEnum(value, "Yellow");
    assert(result);
    assert(value == Yellow);

    result = ToEnum(value, "Green");
    assert(!result);
}

// The ToString and FromString functions can be used to convert between enum
// values and their names. ToString and FromString throw when they encounter
// things they cannot convert.
void ConversionsToFromString()
{
    std::string name = ToString(Yellow);
    assert(name == "Yellow");

    try
    {
        name = ToString(static_cast<Color>(100));
        assert(false); // expected exception
    }
    catch (const bond::CoreException&)
    {
        // ToString throws on unknown values. FromEnum is a non-throwing
        // alternative.
    }

    Color value;
    FromString("Yellow", value);
    assert(value == Yellow);

    try
    {
        FromString("Green", value);
        assert(false); // expected exception
    }
    catch (const bond::CoreException&)
    {
        // FromString throws on unknown names. ToEnum is a non-throwing
        // alternative.
    }
}
