#pragma once

#include <boost/version.hpp>
#include <boost/test/unit_test.hpp>

#define UT_AssertIsTrue(...) BOOST_CHECK((__VA_ARGS__)) 

#define UT_AssertIsFalse(...) BOOST_CHECK(!(__VA_ARGS__))

#define UT_AssertAreEqual BOOST_CHECK_EQUAL

#define UT_AssertThrows BOOST_CHECK_THROW


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
        char id[20];    
        sprintf(id, "0x%x ", key);

        if (!suite)
        {
            suite = BOOST_TEST_SUITE(name);
            parent.add(suite);    
        }

        suite->add(MAKE_BOOST_TEST_CASE(func, id + test));
    }

private:
    std::string name;
    boost::unit_test::test_suite& parent;
    boost::unit_test::test_suite* suite;
};


// Unspecialized unit test wrappers
template <bool Enable, typename Test>
struct TestCase
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test::Run, key, name);  
    }
};

template <bool Enable, template <typename T1> class Test, typename T1>
struct TestCase1
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test<T1>::Run, key, name);
    }
};

template <bool Enable, template <typename T1, typename T2> class Test, typename T1, typename T2>
struct TestCase2
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test<T1, T2>::Run, key, name);
    }
};

template <bool Enable, template <typename T1, typename T2, typename T3> class Test, typename T1, typename T2, typename T3>
struct TestCase3
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test<T1, T2, T3>::Run, key, name);
    }
};

template <bool Enable, template <typename T1, typename T2, typename T3, typename T4> class Test, typename T1, typename T2, typename T3, typename T4>
struct TestCase4
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test<T1, T2, T3, T4>::Run, key, name);
    }
};

template <bool Enable, template <typename T1, typename T2, typename T3, typename T4, typename T5> class Test, typename T1, typename T2, typename T3, typename T4, typename T5>
struct TestCase5
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test<T1, T2, T3, T4, T5>::Run, key, name);
    }
};

template <bool Enable, template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6> class Test, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
struct TestCase6
{
    static void Register(UnitTestSuite& suite, uint32_t key, const char* name)
    {
        suite.Add(Test<T1, T2, T3, T4, T5, T6>::Run, key, name);
    }
};


// Unit test wrapper specialization for disabled tests
template <typename Test>
struct TestCase<false, Test>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};

template <template <typename T1> class Test, typename T1>
struct TestCase1<false, Test, T1>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};


template <template <typename T1, typename T2> class Test, typename T1, typename T2>
struct TestCase2<false, Test, T1, T2>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};


template <template <typename T1, typename T2, typename T3> class Test, typename T1, typename T2, typename T3>
struct TestCase3<false, Test, T1, T2, T3>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};

template <template <typename T1, typename T2, typename T3, typename T4> class Test, typename T1, typename T2, typename T3, typename T4>
struct TestCase4<false, Test, T1, T2, T3, T4>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};

template <template <typename T1, typename T2, typename T3, typename T4, typename T5> class Test, typename T1, typename T2, typename T3, typename T4, typename T5>
struct TestCase5<false, Test, T1, T2, T3, T4 ,T5>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};

template <template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6> class Test, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
struct TestCase6<false, Test, T1, T2, T3, T4 ,T5, T6>
{
    static void Register(UnitTestSuite& /*suite*/, uint32_t /*key*/, const char* /*name*/)
    {
    }
};


// Helper function to add a unit test
template <bool Pred, uint32_t Key, typename Test>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase<ENABLE(Pred, Key), Test>().Register(suite, Key, name);
}

template <bool Pred, uint32_t Key, template <typename T1> class Test, typename T1>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase1<ENABLE(Pred, Key), Test, T1>().Register(suite, Key, name);
}

template <bool Pred, uint32_t Key, template <typename T1, typename T2> class Test, typename T1, typename T2>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase2<ENABLE(Pred, Key), Test, T1, T2>().Register(suite, Key, name);
}

template <bool Pred, uint32_t Key, template <typename T1, typename T2, typename T3> class Test, typename T1, typename T2, typename T3>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase3<ENABLE(Pred, Key), Test, T1, T2, T3>().Register(suite, Key, name);
}

template <bool Pred, uint32_t Key, template <typename T1, typename T2, typename T3, typename T4> class Test, typename T1, typename T2, typename T3, typename T4>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase4<ENABLE(Pred, Key), Test, T1, T2, T3, T4>().Register(suite, Key, name);
}

template <bool Pred, uint32_t Key, template <typename T1, typename T2, typename T3, typename T4, typename T5> class Test, typename T1, typename T2, typename T3, typename T4, typename T5>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase5<ENABLE(Pred, Key), Test, T1, T2, T3, T4, T5>().Register(suite, Key, name);
}

template <bool Pred, uint32_t Key, template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6> class Test, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
inline void AddTestCase(UnitTestSuite& suite, const char* name)
{
    TestCase6<ENABLE(Pred, Key), Test, T1, T2, T3, T4, T5, T6>().Register(suite, Key, name);
}

