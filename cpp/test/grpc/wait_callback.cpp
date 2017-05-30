// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"

#include <bond/core/bond.h>
#include <bond/core/bond_reflection.h>
#include <bond/ext/detail/event.h>
#include <bond/ext/grpc/wait_callback.h>
#include <bond/protocol/compact_binary.h>
#include <bond/stream/output_buffer.h>

#include <atomic>
#include <thread>

namespace wait_callback_tests
{
    using wait_callbackBox = bond::ext::gRPC::wait_callback<bond::Box<int>>;

    static const int ANY_INT_VALUE = 100;
    static bond::bonded<bond::Box<int>> anyBondedValue;
    static grpc::Status anyStatus;

    static bond::bonded<bond::Box<int>> MakeAnyBonded()
    {
        bond::Box<int> boxedInt;
        boxedInt.value = ANY_INT_VALUE;

        bond::OutputBuffer ob;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(ob);
        bond::Serialize(boxedInt, writer);

        bond::blob buffer = ob.GetBuffer();

        bond::InputBuffer ib(buffer);
        bond::CompactBinaryReader<bond::InputBuffer> reader(ib);

        return bond::bonded<bond::Box<int>>(reader);
    }

    static void CallbackCapturesValues()
    {
        wait_callbackBox cb;
        cb(anyBondedValue, anyStatus);

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    static void SubsequentInvocationThrow()
    {
        wait_callbackBox cb;
        cb(anyBondedValue, anyStatus);

        UT_AssertThrows(cb(anyBondedValue, grpc::Status::CANCELLED), bond::ext::gRPC::MultipleInvocationException);

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    static void SubsequentInvocationOnCopyThrow()
    {
        wait_callbackBox cb;
        wait_callbackBox otherCb(cb);
        cb(anyBondedValue, anyStatus);

        UT_AssertThrows(otherCb(anyBondedValue, grpc::Status::CANCELLED), bond::ext::gRPC::MultipleInvocationException);

        UT_AssertIsTrue(otherCb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(otherCb.status().ok());
    }

    static void CanBeConvertedToStdFunction()
    {
        wait_callbackBox cb;
        std::function<void(const bond::bonded<bond::Box<int>>&, const grpc::Status&)> f = cb;

        f(anyBondedValue, anyStatus);

        UT_AssertIsTrue(cb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(cb.status().ok());
    }

    static void CopiesSeeSameValues()
    {
        wait_callbackBox cb;
        wait_callbackBox otherCb(cb);

        cb(anyBondedValue, anyStatus);

        UT_AssertIsTrue(otherCb.response().Deserialize().value == ANY_INT_VALUE);
        UT_AssertIsTrue(otherCb.status().ok());
    }

    static void AsignmentSeesSameValues()
    {
        wait_callbackBox cb;
        cb(anyBondedValue, anyStatus);

        wait_callbackBox otherCb;
        otherCb(anyBondedValue, grpc::Status::CANCELLED);

        UT_AssertIsTrue(!otherCb.status().ok());

        otherCb = cb;
        UT_AssertIsTrue(otherCb.status().ok());
    }

    static void WaitReturnsTrueAfterCBInvoked()
    {
        wait_callbackBox cb;

        bool wasInvoked = cb.wait_for(std::chrono::milliseconds(0));
        UT_AssertIsFalse(wasInvoked);

        cb(anyBondedValue, anyStatus);
        wasInvoked = cb.wait_for(std::chrono::milliseconds(0));
        UT_AssertIsTrue(wasInvoked);
    }

    static void WaitingThreadGetsNotified()
    {
        wait_callbackBox cb;
        bond::ext::detail::event threadStarted;
        std::atomic<bool> wasInvoked(false);


        std::thread t([&cb, &threadStarted, &wasInvoked]()
        {
            threadStarted.set();
            wasInvoked = cb.wait_for(std::chrono::seconds(30));
        });

        // This is a clumsy attempt to get the thread into the wait_for method
        // before invoking the callback.
        bool wasStarted = threadStarted.wait(std::chrono::seconds(30));
        UT_AssertIsTrue(wasStarted);

        cb(anyBondedValue, anyStatus);
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
