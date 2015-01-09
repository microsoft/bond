#define BOOST_TEST_ALTERNATIVE_INIT_API
#include <boost/test/unit_test.hpp>

extern bool init_unit_test();

int main(int argc, char* argv[])
{
    return ::boost::unit_test::unit_test_main(&init_unit_test, argc, argv);
}
