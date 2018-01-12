#include "precompiled.h"

#include <bond/core/detail/checked.h>

BOOST_AUTO_TEST_SUITE(CheckedOperationsTests)

using all_unsigned_types = boost::mpl::list<
    // int8_t, // Signed types will fail compilation
    // float,  // Non-integer types will fail compilation
    uint8_t,
    uint16_t,
    uint32_t,
    uint64_t>;

using all_small_unsigned_types = boost::mpl::list<
    // int8_t,   // Signed types will fail compilation
    // float,    // Non-integer types will fail compilation
    // uint64_t, // 64-bit unsigned will fail compilation, not implemented
    uint8_t,
    uint16_t,
    uint32_t>;

BOOST_AUTO_TEST_CASE_TEMPLATE(CheckedAddUIntTests, T, all_unsigned_types)
{
    using bond::detail::checked_add;

    T x1 = (std::numeric_limits<T>::max)();
    T x2 = (std::numeric_limits<T>::max)() - 50;
    T x3 = 1;
    T x4 = 100;

    BOOST_CHECK_THROW(checked_add(x1, x2), std::overflow_error);
    BOOST_CHECK_THROW(checked_add(x1, x3), std::overflow_error);
    BOOST_CHECK_THROW(checked_add(x1, x4), std::overflow_error);
    BOOST_CHECK_EQUAL(checked_add(x2, x3), x2 + x3);
    BOOST_CHECK_THROW(checked_add(x2, x4), std::overflow_error);
    BOOST_CHECK_EQUAL(checked_add(x3, x4), x3 + x4);

    uint8_t y3 = 1;
    uint8_t y4 = 100;

    BOOST_CHECK_THROW(checked_add(x1, y3), std::overflow_error);
    BOOST_CHECK_THROW(checked_add(x1, y4), std::overflow_error);
    BOOST_CHECK_EQUAL(checked_add(x2, y3), x2 + y3);
    BOOST_CHECK_THROW(checked_add(x2, y4), std::overflow_error);
}

BOOST_AUTO_TEST_CASE(CheckedAddPtrTests)
{
    using bond::detail::checked_add;

    const char* max = reinterpret_cast<const char*>((std::numeric_limits<std::uintptr_t>::max)());
    const char* close = reinterpret_cast<const char*>((std::numeric_limits<std::uintptr_t>::max)() - 50);
    const char* zero = reinterpret_cast<const char*>(0);

    uint32_t one = 1;
    uint32_t hundred = 100;

    BOOST_CHECK_THROW(checked_add(max, one), std::overflow_error);
    BOOST_CHECK_THROW(checked_add(max, hundred), std::overflow_error);
    BOOST_CHECK(checked_add(close, one) == close + one);
    BOOST_CHECK_THROW(checked_add(close, hundred), std::overflow_error);
    BOOST_CHECK(checked_add(zero, one) == zero + one);
    BOOST_CHECK(checked_add(zero, hundred) == zero + hundred);
}

BOOST_AUTO_TEST_CASE_TEMPLATE(CheckedMultiplyTests, T, all_small_unsigned_types)
{
    BOND_CONSTEXPR_OR_CONST auto max_t = (std::numeric_limits<T>::max)();
    BOND_CONSTEXPR_OR_CONST auto max_uint8 = (std::numeric_limits<uint8_t>::max)();
    BOND_CONSTEXPR_OR_CONST T x = max_t / 10;

    using bond::detail::checked_multiply;

    BOOST_CHECK_EQUAL(checked_multiply(max_t, 1), max_t);
    BOOST_CHECK_THROW(checked_multiply(max_t, max_uint8), std::overflow_error);
    BOOST_CHECK_THROW(checked_multiply(max_t, max_uint8 / 2), std::overflow_error);

    BOOST_CHECK_EQUAL(checked_multiply(x, 1), x);
    BOOST_CHECK_THROW(checked_multiply(x, 1 + uint8_t(max_t / x)), std::overflow_error);
    BOOST_CHECK_EQUAL(checked_multiply(T(max_t / max_uint8), max_uint8), max_t);
    BOOST_CHECK_THROW(checked_multiply(T(1 + max_t / max_uint8), max_uint8), std::overflow_error);
}

BOOST_AUTO_TEST_SUITE_END()

bool init_unit_test()
{
    return true;
}
