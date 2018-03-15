#define BOOST_TEST_NO_MAIN
#define BOOST_TEST_ALTERNATIVE_INIT_API
/*
 * We need to include different boost unit test headers on Windows and *nix.
 *
 * Using boost/test/unit_test.hpp on Windows causes linker errors.
 *
 * Using boost/test/included/unit_test.hpp on *nix causes segfaults during
 * teardown in some suites.
 */

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4702) // C4702: unreachable code
#endif

#if defined(_WIN32)
    #include <boost/test/included/unit_test.hpp>
#else
    #include <boost/test/unit_test.hpp>
#endif

#ifdef _MSC_VER
#pragma warning(pop)
#endif

extern bool init_unit_test();

int main(int argc, char* argv[])
{
    return ::boost::unit_test::unit_test_main(&init_unit_test, argc, argv);
}
