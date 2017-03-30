// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <bond/ext/grpc/thread_pool.h>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"
#include "test_utils_grpc.h"

#include <boost/chrono.hpp>
#include <atomic>
#include <functional>

class BasicThreadPoolTests
{
    static
    void addOne(int* i, event* sum_event)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        (*i)++;
        sum_event->set();
    }

    static
    void UseStdFunction()
    {
        bond::ext::thread_pool threads(1);
        int sum = 0;
        event sum_event;

        std::function<void(int*, event*)> f_addOne = addOne;

        threads.schedule(std::bind(f_addOne, &sum, &sum_event));

        bool waitResult = sum_event.wait(std::chrono::seconds(30));

        UT_AssertIsTrue(waitResult);
    }

    static
    void FinishAllTasksAfterDelete()
    {
        std::unique_ptr<bond::ext::thread_pool> threads(new bond::ext::thread_pool(2));
        std::atomic<int> sum(0);

        auto increment = [&sum](){
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            sum++;
        };

        threads->schedule(increment);
        threads->schedule(increment);
        threads->schedule(increment);
        threads->schedule(increment);

        // blocks until all schedule tasks are finished
        threads.reset();

        UT_AssertIsTrue(sum == 4);
    }

public:

    static
    void Initialize()
    {
        UnitTestSuite suite("ThreadPool");

        suite.AddTestCase(UseStdFunction, "UseStdFunction");
        suite.AddTestCase(FinishAllTasksAfterDelete, "FinishAllTasksAfterDelete");
    }
};

bool init_unit_test()
{
    BasicThreadPoolTests::Initialize();
    return true;
}
