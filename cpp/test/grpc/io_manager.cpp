// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#ifdef _MSC_VER
    #pragma warning(disable : 4100) // disable "unreferenced formal parameter" warning
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <grpc++/grpc++.h>
#include <grpc++/alarm.h>
#include <grpc++/impl/codegen/completion_queue.h>
#include <grpc++/impl/grpc_library.h>
#include <grpc/grpc.h>
#include <grpc/support/time.h>

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/detail/io_manager_tag.h>
#include <bond/ext/detail/countdown_event.h>
#include <bond/ext/detail/barrier.h>
#include <bond/ext/detail/event.h>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"

#include <boost/chrono.hpp>
#include <boost/test/debug.hpp>
#include <atomic>
#include <memory>

using namespace bond::ext::detail;
using namespace bond::ext::gRPC::detail;
using namespace bond::ext::gRPC;

class io_managerTests
{
    static void PollOneItem()
    {
        io_manager ioManager(std::unique_ptr<grpc::CompletionQueue>(new grpc::CompletionQueue));
        ioManager.start();

        struct alarm_completion_tag : io_manager_tag
        {
            event& _e;

            explicit alarm_completion_tag(event& e) : _e(e) { }

            void invoke(bool) override
            {
                _e.set();
            }
        };

        event alarmCompleted;
        alarm_completion_tag act(alarmCompleted);

        gpr_timespec deadline = gpr_time_0(GPR_CLOCK_MONOTONIC);
        grpc::Alarm alarm(ioManager.cq(), deadline, &act);

        bool wasSet = alarmCompleted.wait(std::chrono::seconds(30));
        UT_AssertIsTrue(wasSet);
    }

    static void PollManyItems()
    {
        io_manager ioManager(std::unique_ptr<grpc::CompletionQueue>(new grpc::CompletionQueue));
        ioManager.start();

        const size_t numItems = 1000;
        countdown_event ce(numItems);

        struct alarm_completion_tag : io_manager_tag
        {
            countdown_event& _ce;

            explicit alarm_completion_tag(countdown_event& ce) : _ce(ce) { }

            void invoke(bool) override
            {
                _ce.set();
            }
        };

        alarm_completion_tag act(ce);

        const gpr_timespec deadline = gpr_time_0(GPR_CLOCK_MONOTONIC);

        std::vector<grpc::Alarm> alarms;
        alarms.reserve(numItems);
        for (size_t i = 0; i < numItems; ++i)
        {
            alarms.emplace_back(ioManager.cq(), deadline, &act);
        }

        bool wasSet = ce.wait(std::chrono::seconds(30));
        UT_AssertIsTrue(wasSet);
    }

    static void ShutdownUnstarted()
    {
        io_manager ioManager(std::unique_ptr<grpc::CompletionQueue>(new grpc::CompletionQueue));
        ioManager.shutdown();
        ioManager.wait();

        // also tests that we can run the dtor after successful shutdown
    }

    static void ConcurrentShutdown()
    {
        io_manager ioManager(std::unique_ptr<grpc::CompletionQueue>(new grpc::CompletionQueue));
        ioManager.start();

        const size_t numConcurrentShutdowns = 5;
        barrier threadsStarted(numConcurrentShutdowns);
        barrier threadsObservedShutdown(numConcurrentShutdowns);

        std::vector<std::thread> threads;
        threads.reserve(5);
        for (size_t i = 0; i < numConcurrentShutdowns; ++i)
        {
            threads.emplace_back([&ioManager, &threadsStarted, &threadsObservedShutdown]()
            {
                threadsStarted.enter();

                ioManager.shutdown();
                ioManager.wait();

                threadsObservedShutdown.enter();
            });
        }

        bool wasSet = threadsStarted.wait(std::chrono::seconds(30));
        UT_AssertIsTrue(wasSet); // all the threads took too long to get started

        wasSet = threadsObservedShutdown.wait(std::chrono::seconds(30));
        UT_AssertIsTrue(wasSet); // took too long to see the io_manager shutdown

        for (auto& thread : threads)
        {
            thread.join();
        }
    }

public:
    static void Initialize()
    {
        UnitTestSuite suite("io_manager");
        suite.AddTestCase(PollOneItem, "PollOneItem");
        suite.AddTestCase(PollManyItems, "PollManyItems");
        suite.AddTestCase(ShutdownUnstarted, "ShutdownUnstarted");
        suite.AddTestCase(ConcurrentShutdown, "ConcurrentShutdown");
    }
};

bool init_unit_test()
{
    // grpc allocates a bunch of stuff on-demand caused the leak tracker to
    // report leaks. Disable it for this test.
    boost::debug::detect_memory_leaks(false);

    // Initialize the gRPC++ library
    grpc::internal::GrpcLibraryInitializer initializer;
    initializer.summon();

    io_managerTests::Initialize();
    return true;
}
