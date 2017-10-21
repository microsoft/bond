// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/bond.h>
#include <bond/core/bond_reflection.h>
#include <bond/core/bonded.h>
#include <bond/ext/grpc/shared_unary_call.h>
#include <bond/ext/grpc/unary_call.h>

#include <boost/static_assert.hpp>
#include <boost/test/unit_test.hpp>

#include <type_traits>

BOOST_AUTO_TEST_SUITE(UnaryCallTests)

using bond::ext::gRPC::unary_call;
using bond::ext::gRPC::shared_unary_call;

using unary_callBox = unary_call<bond::bonded<bond::Box<int32_t>>,
                                 bond::Box<int32_t>>;
using shared_unary_callBox = shared_unary_call<bond::bonded<bond::Box<int32_t>>,
                                               bond::Box<int32_t>>;

BOOST_STATIC_ASSERT(!std::is_default_constructible<unary_callBox>::value);
BOOST_STATIC_ASSERT(!std::is_copy_constructible<unary_callBox>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_constructible<unary_callBox>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_assignable<unary_callBox>::value);

BOOST_STATIC_ASSERT(std::is_nothrow_default_constructible<shared_unary_callBox>::value);
BOOST_STATIC_ASSERT(std::is_copy_constructible<shared_unary_callBox>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_constructible<shared_unary_callBox>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_assignable<shared_unary_callBox>::value);

BOOST_AUTO_TEST_CASE(Default_shared_unary_call_IsEmpty)
{
    shared_unary_callBox c{};
    BOOST_CHECK(!static_cast<bool>(c));
}

BOOST_AUTO_TEST_SUITE_END()

bool init_unit_test()
{
    return true;
}
