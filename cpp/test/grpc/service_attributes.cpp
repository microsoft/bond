// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include "services_grpc.h"

#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <iterator>
#include <map>
#include <string>

using unit_test::SimpleService;

static bool AttributeMapsEqual(
    const std::map<std::string, std::string>& lhs,
    const std::map<std::string, std::string>& rhs)
{
    return lhs.size() == rhs.size()
        && std::equal(std::begin(lhs), std::end(lhs), std::begin(rhs));
}

BOOST_AUTO_TEST_SUITE(ServiceAttributesTests)

BOOST_AUTO_TEST_CASE(AttributesOnService_CanBeFound)
{
    static const std::map<std::string, std::string> expected
    {
        { "SomeAttribute", "service value" }
    };

    BOOST_CHECK(
        AttributeMapsEqual(
            expected,
            SimpleService::Schema::metadata.attributes));
}

BOOST_AUTO_TEST_CASE_TEMPLATE(
    AttributesOnMethods_CanBeFound,
    Method,
    SimpleService::Schema::methods)
{
    static const std::map<std::string, std::map<std::string, std::string>> expectedPerMethod
    {
        {
            "IntToInt",
            {
                { "SomeAttribute", "method value" },
                { "DifferentAttribute", "" }
            }
        },
        {
            "NothingToInt",
            {
                {"NothingToInt", "no clash"}
            }
        },
        { "IntToNothing", { } },
        {"NothingToNothing", { } }
    };

    const auto expected = expectedPerMethod.find(Method::metadata.name);
    BOOST_CHECK(expected != std::end(expectedPerMethod));
    BOOST_CHECK(AttributeMapsEqual(expected->second, Method::metadata.attributes));
}

BOOST_AUTO_TEST_SUITE_END()

bool init_unit_test()
{
    return true;
}
