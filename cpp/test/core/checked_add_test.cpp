#include "precompiled.h"

#include <bond/core/detail/checked_add.h>

struct CheckedAddUIntTest
{
    template <typename X>
    void operator()(const X&)
    {
        X x1 = (std::numeric_limits<X>::max)();
        X x2 = (std::numeric_limits<X>::max)() - 50;
        X x3 = 1;
        X x4 = 100;

        UT_AssertThrows(bond::detail::checked_add(x1, x2), std::overflow_error);
        UT_AssertThrows(bond::detail::checked_add(x1, x3), std::overflow_error);
        UT_AssertThrows(bond::detail::checked_add(x1, x4), std::overflow_error);
        UT_AssertAreEqual(bond::detail::checked_add(x2, x3), x2 + x3);
        UT_AssertThrows(bond::detail::checked_add(x2, x4), std::overflow_error);
        UT_AssertAreEqual(bond::detail::checked_add(x3, x4), x3 + x4);
    }
};

TEST_CASE_BEGIN(CheckedAddUIntTests)
{
    typedef boost::mpl::list<
        // int8_t, // Signed types will fail compilation
        // float,  // Non-integer types will fail compilation
        uint8_t,
        uint16_t,
        uint32_t,
        uint64_t
    > Types;

    boost::mpl::for_each<Types>(CheckedAddUIntTest());
}
TEST_CASE_END

TEST_CASE_BEGIN(CheckedAddPtrTests)
{
    const char* max = reinterpret_cast<const char*>((std::numeric_limits<std::uintptr_t>::max)());
    const char* close = reinterpret_cast<const char*>((std::numeric_limits<std::uintptr_t>::max)() - 50);
    const char* zero = reinterpret_cast<const char*>(0);

    uint32_t one = 1;
    uint32_t hundred = 100;

    UT_AssertThrows(bond::detail::checked_add(max, one), std::overflow_error);
    UT_AssertThrows(bond::detail::checked_add(max, hundred), std::overflow_error);
    UT_AssertIsTrue(bond::detail::checked_add(close, one) == (close + one));
    UT_AssertThrows(bond::detail::checked_add(close, hundred), std::overflow_error);
    UT_AssertIsTrue(bond::detail::checked_add(zero, one) == (zero + one));
    UT_AssertIsTrue(bond::detail::checked_add(zero, hundred) == (zero + hundred));
}
TEST_CASE_END

void Initialize()
{
    UnitTestSuite suite("checked_add tests");
    AddTestCase<TEST_ID(0x2501), CheckedAddUIntTests>(suite, "unsigned integer checked_add tests");
    AddTestCase<TEST_ID(0x2502), CheckedAddPtrTests>(suite, "pointer checked_add tests");
}

bool init_unit_test()
{
    Initialize();
    return true;
}

