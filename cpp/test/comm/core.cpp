#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <comm_test_common_apply.h>
#include <comm_test_common_reflection.h>
#include <comm_test_common_comm.h>

#include <bond/comm/timeout.h>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"
#include "bonded_cast.h"
#include "test_utils_comm.h"

static const uint32_t ITERATIONS = 2;

using namespace unittest::comm;

namespace TestCore
{
    static
    void WaitForResponseForgotten()
    {
        //
        // Validate common WaitForResponse usage patterns.
        //
        {
            //
            // Safe to destroy unused waiters.
            //
            WaitForResponse<Params> w1;

            WaitForResponse<Params> w2;

            auto callback = w2.Callback();
        }

        {
            //
            // Safe to call forgotten waiters.
            //

            std::function<void (const bond::comm::message<Params>&)> callback;

            {
                WaitForResponse<Params> w;
                callback = w.Callback();;
            }

            Params params;
            callback(boost::cref(params));
        }
    }

    static
    void WaitForResponseMainUsage()
    {
        //
        // Main usage patterns.
        //
        {
            //
            // Same thread.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                w.Callback()(bond::comm::bonded_cast<Params>(in));

                w.Deserialize(out);

                UT_AssertIsTrue(out.x == i);
            }
        }

        {
            //
            // Different thread.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                boost::scoped_thread<boost::join_if_joinable> thread(boost::bind(w.Callback(), bond::comm::bonded_cast<Params>(in)));

                w.Deserialize(out);

                UT_AssertIsTrue(out.x == i);
            }
        }

        {
            //
            // Same thread, with large timeout.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                w.Callback()(bond::comm::bonded_cast<Params>(in));

                w.Deserialize(out, 1000000);

                UT_AssertIsTrue(out.x == i);
            }
        }

        {
            //
            // Different thread, with large timeout.
            //
            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                Params in, out;
                in.x = i;

                boost::scoped_thread<boost::join_if_joinable> thread(boost::bind(w.Callback(), bond::comm::bonded_cast<Params>(in)));

                w.Deserialize(out, 10000000);

                UT_AssertIsTrue(out.x == i);
            }
        }
    }

    static
    void WaitForResponseTimeout()
    {
        //
        // Usage patterns with timeout triggered.
        //
        {
            //
            // Same thread.
            //

            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                auto callback = w.Callback();

                Params out;

                try
                {
                    w.Deserialize(out, 1);

                    UT_AssertIsTrue(false);
                }
                catch (const bond::Exception&)
                {
                }
            }
        }

        {
            //
            // Different thread.
            //
            WaitForResponse<Params> w;

            for (uint32_t i = 1; i < ITERATIONS; ++i)
            {
                //
                // Wait response can be reused.
                //
                auto callback = w.Callback();

                try
                {
                    Params out;
                    w.Deserialize(out, 1);

                    UT_AssertIsTrue(false);
                }
                catch (const bond::Exception&)
                {
                }

                Params in;
                in.x = i;

                boost::scoped_thread<boost::join_if_joinable> thread(boost::bind(callback, bond::comm::bonded_cast<Params>(in)));
            }
        }
    }


    static
    void ScheduleTimeoutForgotten()
    {
        bond::comm::thread_pool threads;

        auto callback =
            bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                               [](const bond::comm::message<void>&)
                                               {
                                               },
                                               10000);
    }


    static
    void ScheduleTimeoutMainCallback()
    {
        bond::comm::thread_pool threads;
        {
            bool flag = false;
            auto callback =
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&flag](const bond::comm::message<void>& m)
                                                   {
                                                       flag = !m.is_error();
                                                   },
                                                   10000);

            callback(bond::comm::message<void>());
            UT_AssertIsTrue(flag);
        }
    }

    static
    void ScheduleTimeoutElapsed()
    {
        bond::comm::thread_pool threads;
        {
            bool flag = false;
            auto callback =
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&flag](const bond::comm::message<void>& m)
                                                   {
                                                       flag = m.is_error();
                                                   },
                                                   1);
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            callback(bond::comm::message<void>());
            UT_AssertIsTrue(flag);
        }

        {
            bool flag = false;
            auto callback =
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&flag](const bond::comm::message<void>& m)
                                                   {
                                                       flag = m.is_error();
                                                   },
                                                   1);
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            UT_AssertIsTrue(flag);
        }

        {
            //
            // Test if callback may be released within timer thread.
            //
            bool flag = false;
            std::function<void (const bond::comm::message<void>&)>* callback = nullptr;
            callback = new std::function<void (const bond::comm::message<void>&)>(
                bond::comm::schedule_timeout<void>(threads.get_io_service(),
                                                   [&callback, &flag](const bond::comm::message<void>& m)
                                                   {
                                                        delete callback;
                                                        flag = m.is_error();
                                                   },
                                                   10));

            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            UT_AssertIsTrue(flag);
        }
    }


    static
    void Initialize()
    {
        UnitTestSuite suite("Generic RPC utils");

        suite.AddTestCase(WaitForResponseForgotten, "WaitForResponseForgotten");
        suite.AddTestCase(WaitForResponseMainUsage, "WaitForResponseMainUsage");
        suite.AddTestCase(WaitForResponseTimeout, "WaitForResponseTimeout");

        suite.AddTestCase(ScheduleTimeoutForgotten, "ScheduleTimeoutForgotten");
        suite.AddTestCase(ScheduleTimeoutMainCallback, "ScheduleTimeoutMainCallback");
        suite.AddTestCase(ScheduleTimeoutElapsed, "ScheduleTimeoutElapsed");
    }
}

bool init_unit_test()
{
    TestCore::Initialize();
    return true;
}
