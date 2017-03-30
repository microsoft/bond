// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <chrono>
#include <condition_variable>
#include <mutex>

class event
{
public:
    explicit event(bool initialValue = false) :
        _m(),
        _cv(),
        _set(initialValue)
    {}

    void set()
    {
        setResetImpl(true);
    }

    void reset()
    {
        setResetImpl(false);
    }

    void wait()
    {
        std::unique_lock<std::mutex> lock(_m);
        return _cv.wait(lock, [this]() {return _set;});
    }

    template <typename Rep, typename Period>
    bool wait(std::chrono::duration<Rep, Period> timeout)
    {
        std::unique_lock<std::mutex> lock(_m);
        return _cv.wait_for(lock, timeout, [this]() {return _set;});
    }

private:
    std::mutex _m;
    std::condition_variable _cv;
    bool _set;

    event(const event&) = delete;
    event(event&&) = delete;
    event& operator=(const event&) = delete;
    event& operator=(event&&) = delete;

    void setResetImpl(bool value)
    {
        {
            std::lock_guard<std::mutex> lock(_m);
            _set = value;
        }

        _cv.notify_all();
    }
};

