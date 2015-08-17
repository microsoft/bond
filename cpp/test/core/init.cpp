#define BOOST_TEST_NO_MAIN

#include "precompiled.h"

#include "protocol_test.h"
#include "serialization_test.h"
#include "bonded_tests.h"
#include "skip_tests.h"
#include "basic_tests.h"
#include "container_extensibility.h"
#include "metadata_tests.h"
#include "json_tests.h"
#include "exception_tests.h"
#include "apply_tests.h"
#include "maybe_tests.h"
#include "validate_tests.h"
#include "cmdargs.h"

bool init_unit_test()
{
    MaybeTest::Initialize();
    ExceptionTest::Initialize();
    MetadataTest::Initialize();
    SkipTest::InitializeMismatchedIdTests();
    SkipTest::InitializeMismatchedTypeTests();
    ProtocolTest::Initialize();
    SerializationTest::Initialize();
    BondedTest::Initialize();
    BasicTest::Initialize();
    ExtensibilityTest::Initialize();
    ExtensibilityTest::InitializeAssociative();
    JSONTest::Initialize();
    ApplyTest::Initialize();
    ValidateTest::Initialize();
    CmdArgs::Initialize();
    return true;
}

boost::unit_test::test_suite* init_unit_test_suite(int, char*[])
{
    init_unit_test();
    return 0;
}
