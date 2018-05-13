// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"

#include <bond/core/bond.h>
#include <bond/core/bond_reflection.h>
#include <bond/ext/grpc/wait_callback.h>
#include <bond/protocol/compact_binary.h>
#include <bond/stream/output_buffer.h>

#include "event.h"

#include <boost/static_assert.hpp>

#include <atomic>
#include <thread>
#include <type_traits>

namespace wait_callback_tests
{
    using test_struct_type = bond::Box<int>;

    BOOST_STATIC_ASSERT(std::is_copy_constructible<bond::ext::gRPC::unary_call_result<test_struct_type>>::value);
    BOOST_STATIC_ASSERT(std::is_move_constructible<bond::ext::gRPC::unary_call_result<test_struct_type>>::value);

    using test_wait_callback = bond::ext::gRPC::wait_callback<test_struct_type>;

    static const int ANY_INT_VALUE = 100;
    static bond::bonded<test_struct_type> anyBondedValue;
    static grpc::Status anyStatus;

    static bond::bonded<test_struct_type> MakeAnyBonded()
    {
        test_struct_type boxedInt;
        boxedInt.value = ANY_INT_VALUE;

        bond::OutputBuffer ob;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(ob);
        bond::Serialize(boxedInt, writer);

        bond::blob buffer = ob.GetBuffer();

        bond::InputBuffer ib(buffer);
        bond::CompactBinaryReader<bond::InputBuffer> reader(ib);

        return bond::bonded<test_struct_type>(reader);
    }

    static void CallbackCapturesValues()
    {
        test_wait_callback cb;
        cb({ anyBondedValue, anyStatus });

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    static void SubsequentInvocationThrow()
    {
        test_wait_callback cb;
        cb({ anyBondedValue, anyStatus });

        bond::ext::gRPC::unary_call_result<test_struct_type> args2{ anyBondedValue, grpc::Status::CANCELLED };

        UT_AssertThrows(cb(std::move(args2)), bond::ext::gRPC::MultipleInvocationException);

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    static void SubsequentInvocationOnCopyThrow()
    {
        test_wait_callback cb;
        test_wait_callback otherCb(cb);

        cb({ anyBondedValue, anyStatus });

        bond::ext::gRPC::unary_call_result<test_struct_type> args2{ anyBondedValue, grpc::Status::CANCELLED };

        UT_AssertThrows(otherCb(std::move(args2)), bond::ext::gRPC::MultipleInvocationException);

        UT_AssertIsTrue(otherCb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(otherCb.status().ok());
    }

    static void CanBeConvertedToStdFunction()
    {
        test_wait_callback cb;
        std::function<void(bond::ext::gRPC::unary_call_result<test_struct_type>)> f = cb;

        f({ anyBondedValue, anyStatus });

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    static void CopiesSeeSameValues()
    {
        test_wait_callback cb;
        test_wait_callback otherCb(cb);

        cb({ anyBondedValue, anyStatus });

        UT_AssertIsTrue(otherCb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(otherCb.status().ok());
    }

    static void AsignmentSeesSameValues()
    {
        test_wait_callback cb;
        cb({ anyBondedValue, anyStatus });

        test_wait_callback otherCb;
        bond::ext::gRPC::unary_call_result<test_struct_type> args2{ anyBondedValue, grpc::Status::CANCELLED };
        otherCb(std::move(args2));

        UT_AssertIsTrue(!otherCb.status().ok());

        otherCb = cb;
        UT_AssertIsTrue(otherCb.status().ok());
    }

    static void WaitReturnsTrueAfterCBInvoked()
    {
        test_wait_callback cb;

        bool wasInvoked = cb.wait_for(std::chrono::milliseconds(0));
        UT_AssertIsFalse(wasInvoked);

        cb({ anyBondedValue, anyStatus });
        wasInvoked = cb.wait_for(std::chrono::milliseconds(0));
        UT_AssertIsTrue(wasInvoked);
    }

    static void WaitingThreadGetsNotified()
    {
        test_wait_callback cb;
        unit_test::event threadStarted;
        std::atomic<bool> wasInvoked(false);


        std::thread t([&cb, &threadStarted, &wasInvoked]()
        {
            threadStarted.set();
            wasInvoked = cb.wait_for(std::chrono::seconds(30));
        });

        // This is a clumsy attempt to get the thread into the wait_for method
        // before invoking the callback.
        bool wasStarted = threadStarted.wait_for(std::chrono::seconds(30));
        UT_AssertIsTrue(wasStarted);

        cb({ anyBondedValue, anyStatus });
        t.join();

        UT_AssertIsTrue(wasInvoked);
    }

    static void Initialize()
    {
        anyBondedValue = MakeAnyBonded();
        anyStatus = grpc::Status::OK;

        UnitTestSuite suite("wait_callback");
        suite.AddTestCase(&CallbackCapturesValues, "CallbackCapturesValues");
        suite.AddTestCase(&SubsequentInvocationThrow, "SubsequentInvocationThrow");
        suite.AddTestCase(&SubsequentInvocationOnCopyThrow, "SubsequentInvocationOnCopyThrow");
        suite.AddTestCase(&CanBeConvertedToStdFunction, "CanBeConvertedToStdFunction");
        suite.AddTestCase(&CopiesSeeSameValues, "CopiesSeeSameValues");
        suite.AddTestCase(&AsignmentSeesSameValues, "AsignmentSeesSameValues");
        suite.AddTestCase(&WaitReturnsTrueAfterCBInvoked, "WaitReturnsTrueAfterCBInvoked");
        suite.AddTestCase(&WaitingThreadGetsNotified, "WaitingThreadGetsNotified");
    }
}

bool init_unit_test()
{
    wait_callback_tests::Initialize();
    return true;
}
