#pragma once

#include <boost/format.hpp>
#include <boost/version.hpp>
#include <boost/test/unit_test.hpp>

#define UT_AssertIsTrue(...) BOOST_CHECK((__VA_ARGS__))

#define UT_AssertIsFalse(...) BOOST_CHECK(!(__VA_ARGS__))

#define UT_AssertAreEqual BOOST_CHECK_EQUAL

#define UT_AssertThrows BOOST_CHECK_THROW

#define UT_AssertIsNull(...) BOOST_CHECK(nullptr == (__VA_ARGS__))

#define UT_AssertIsNotNull(...) BOOST_CHECK(nullptr != (__VA_ARGS__))

#if defined(BOND_COMPACT_BINARY_PROTOCOL)
#   define  TEST_COMPACT_BINARY_PROTOCOL(...)  __VA_ARGS__
#else
#   define  TEST_COMPACT_BINARY_PROTOCOL(...)
#endif


#if defined(BOND_SIMPLE_BINARY_PROTOCOL)
#   define  TEST_SIMPLE_PROTOCOL(...)  __VA_ARGS__
#else
#   define  TEST_SIMPLE_PROTOCOL(...)
#endif


#if defined(BOND_FAST_BINARY_PROTOCOL)
#   define  TEST_FAST_BINARY_PROTOCOL(...)  __VA_ARGS__
#else
#   define  TEST_FAST_BINARY_PROTOCOL(...)
#endif


#if defined(BOND_SIMPLE_JSON_PROTOCOL)
#   define  TEST_SIMPLE_JSON_PROTOCOL(...)  __VA_ARGS__
#else
#   define  TEST_SIMPLE_JSON_PROTOCOL(...)
#endif


// "Wrap" a function definition in a struct with single static method Run.
// Using structs for test cases rather than functions allows us to refer to
// generic tests without having to instantiate them (via template template
// parameters) which in turn let's us disable individual test cases at compile
// time such that they are not compiled at all.
// The drawback is that we need to wrap test case definition in two macros (one
// macro would be enough for non-generic tests but not for generic ones).
#define TEST_CASE_BEGIN(name) struct name{static void Run()
#define TEST_CASE_END };

// Individual test cases can be enabled by defining ENABLE_TEST_CASE macro to
// be equal to the test case id. Test ids are built from two elements,
// explicitly specified test suite id (high order 16 bits) and sequential test
// case id (low order 16 bits) generated using __COUNTER__. Test ids are
// outputted as part of test case names in the test report.
// Test cases can be also defined conditionally based on a compile-time boolean
// constant (usually derived from a condition on type parameters).
#define KEY(x) ((x << 16) | __COUNTER__)
#define TEST_ID(x) (true),KEY(x)
#define COND_TEST_ID(x, cond) (cond),KEY(x)

#ifndef ENABLE_TEST_CASE
#   define ENABLE(Pred, Key) (Pred)
#else
#   define ENABLE(Pred, Key) (Pred && ENABLE_TEST_CASE == ((ENABLE_TEST_CASE & 0xFFFF) ? Key : (Key & 0xFFFF0000)))
#endif

#if BOOST_VERSION >= 105900
#   define MAKE_BOOST_TEST_CASE(test, name) \
    boost::unit_test::make_test_case(boost::function<void ()>(test), (name), __FILE__, __LINE__)
#else
#   define MAKE_BOOST_TEST_CASE(test, name) \
    boost::unit_test::make_test_case(boost::unit_test::callback0<>(test), (name))
#endif

// Abstracts Boost Test APIs for defining test suites and cases.
class UnitTestSuite
{
public:
    UnitTestSuite(const std::string& name)
        : name(name),
          parent(boost::unit_test::framework::master_test_suite()),
          suite(0)
    {}

    void Add(void (*func)(), uint32_t key, const std::string& test)
    {
        AddTestCase(
            func,
            boost::str(boost::format("%#x %s") % key % test));
    }

    void AddTestCase(void (*func)(), const std::string& test)
    {
        if (!suite)
        {
            suite = BOOST_TEST_SUITE(name);
            parent.add(suite);
        }

        suite->add(MAKE_BOOST_TEST_CASE(func, test));
    }

private:
    std::string name;
    boost::unit_test::test_suite& parent;
    boost::unit_test::test_suite* suite;
};


template <uint32_t Key, typename Test>
inline void AddTestCase(UnitTestSuite& suite, const char* name, std::true_type)
{
    suite.Add(Test::Run, Key, name);
}

template <uint32_t Key, typename Test>
inline void AddTestCase(UnitTestSuite& /*suite*/, const char* /*name*/, std::false_type)
{}

template <uint32_t Key, template <typename...> class Test, typename... T>
inline void AddTestCase(UnitTestSuite& suite, const char* name, std::true_type)
{
    suite.Add(Test<T...>::Run, Key, name);
}

template <uint32_t Key, template <typename...> class Test, typename... T>
inline void AddTestCase(UnitTestSuite& /*suite*/, const char* /*name*/, std::false_type)
{}

template <bool Pred, uint32_t Key, typename Test>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    AddTestCase<Key, Test>(suite, name, std::integral_constant<bool, ENABLE(Pred, Key)>{});
}

template <bool Pred, uint32_t Key, template <typename...> class Test, typename... T>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    AddTestCase<Key, Test, T...>(suite, name, std::integral_constant<bool, ENABLE(Pred, Key)>{});
}
