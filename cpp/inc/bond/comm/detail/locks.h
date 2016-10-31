
#pragma once

#include <mutex>

#if (defined(WIN32) || defined(_WIN32) || defined(__WIN32__))

#include <boost/noncopyable.hpp>
#include <windows.h>

namespace bond { namespace comm
{

class critical_section
    : private boost::noncopyable
{
public:
    critical_section()
    {
        ::InitializeCriticalSection(&m_CS);
    }
    
    ~critical_section()
    {
        ::DeleteCriticalSection(&m_CS);
    }
    
    bool try_lock()
    {
        return ::TryEnterCriticalSection(&m_CS) != 0;
    }
    
    void lock()
    {
        ::EnterCriticalSection(&m_CS);
    }
    
    void unlock()
    {
        ::LeaveCriticalSection(&m_CS);
    }

private:

    CRITICAL_SECTION m_CS;
};

} } // namespace bond.comm

#else

namespace bond { namespace comm
{

using critical_section = std::recursive_mutex;

} } // namespace bond.comm

#endif


namespace bond { namespace comm
{
template <typename T, typename Lock = T>
class lockable_guard
    : public std::unique_lock<Lock>
{
public:
    lockable_guard(lockable_guard&& that)
        : std::unique_lock<Lock>(std::move(that))
        , m_object(that.m_object)
    {}
    
    lockable_guard(T& _lockable)
        : std::unique_lock<Lock>(_lockable)
        , m_object(_lockable.m_object)
    {}

    typename T::lockable_type* operator->()
    {
        return &m_object;
    }

    typename T::lockable_type& operator*()
    {
        return m_object;
    }

private:
    typename T::lockable_type& m_object;
};

template <typename T, typename Lock = critical_section>
class lockable
    : public Lock
{
public:
    typedef T      lockable_type;
    typedef Lock   lock_type;

    lockable()
        : m_object()
    {}

    template <typename U>
    explicit
    lockable(const U& value)
        : m_object(value)
    {}

    template <typename, typename>
    friend class lockable_guard;

private:
    T m_object;
};


template <typename T>
lockable_guard<T> lock(T& _lockable)
{
    return _lockable;
}


template <typename T>
lockable_guard<T, typename T::lock_type::shared> shared_lock(T& _lockable)
{
    return _lockable;
}


template <typename T>
lockable_guard<T, typename T::lock_type::exclusive> exclusive_lock(T& _lockable)
{
    return _lockable;
}

} } // namespace bond.comm
