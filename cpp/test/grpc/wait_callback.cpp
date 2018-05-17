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

    bond::bonded<test_struct_type> MakeAnyBonded()
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

    bond::ext::gRPC::unary_call_result<test_struct_type> MakeCallbackArg(
        const bond::bonded<test_struct_type>& response,
        const grpc::Status& status)
    {
        // We don't want to create a real context to test against. When
        // there's a real context, it has to be cleaned up, but to properly
        // clean it up, some globals in gRPC++ need to still be alive.
        // However, we don't want to deal with making sure that the test
        // globals and the gRPC++ globals are destroyed in the right order.
        // Thus, we test with nullptr.
        return bond::ext::gRPC::unary_call_result<test_struct_type>(response, status, nullptr);
    }

    void CallbackCapturesValues()
    {
        test_wait_callback cb;
        cb(MakeCallbackArg(anyBondedValue, anyStatus));

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    void SubsequentInvocationThrow()
    {
        test_wait_callback cb;
        cb(MakeCallbackArg(anyBondedValue, anyStatus));

        auto args2 = MakeCallbackArg(anyBondedValue, grpc::Status::CANCELLED);

        UT_AssertThrows(cb(std::move(args2)), bond::ext::gRPC::MultipleInvocationException);

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    void SubsequentInvocationOnCopyThrow()
    {
        test_wait_callback cb;
        test_wait_callback otherCb(cb);

        cb(MakeCallbackArg(anyBondedValue, anyStatus));

        auto args2 = MakeCallbackArg(anyBondedValue, grpc::Status::CANCELLED);

        UT_AssertThrows(otherCb(std::move(args2)), bond::ext::gRPC::MultipleInvocationException);

        UT_AssertIsTrue(otherCb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(otherCb.status().ok());
    }

    void CanBeConvertedToStdFunction()
    {
        test_wait_callback cb;
        std::function<void(bond::ext::gRPC::unary_call_result<test_struct_type>)> f = cb;

        f(MakeCallbackArg(anyBondedValue, anyStatus));

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    void CopiesSeeSameValues()
    {
        test_wait_callback cb;
        test_wait_callback otherCb(cb);

        cb(MakeCallbackArg(anyBondedValue, anyStatus));

        UT_AssertIsTrue(otherCb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(otherCb.status().ok());
    }

    void AsignmentSeesSameValues()
    {
        test_wait_callback cb;
        cb(MakeCallbackArg(anyBondedValue, anyStatus));

        test_wait_callback otherCb;
        auto args2 = MakeCallbackArg(anyBondedValue, grpc::Status::CANCELLED);
        otherCb(std::move(args2));

        UT_AssertIsTrue(!otherCb.status().ok());

        otherCb = cb;
        UT_AssertIsTrue(otherCb.status().ok());
    }

    void WaitReturnsTrueAfterCBInvoked()
    {
        test_wait_callback cb;

        bool wasInvoked = cb.wait_for(std::chrono::milliseconds(0));
        UT_AssertIsFalse(wasInvoked);

        cb(MakeCallbackArg(anyBondedValue, anyStatus));
        wasInvoked = cb.wait_for(std::chrono::milliseconds(0));
        UT_AssertIsTrue(wasInvoked);
    }

    void WaitingThreadGetsNotified()
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

        cb(MakeCallbackArg(anyBondedValue, anyStatus));
        t.join();

        UT_AssertIsTrue(wasInvoked);
    }

    void Initialize()
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
