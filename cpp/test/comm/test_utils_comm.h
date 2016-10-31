

#include <bond/core/bond_types.h>
#include <bond/comm/message.h>

#include <boost/noncopyable.hpp>
#include <boost/mpl/if.hpp>

#include <condition_variable>
#include <type_traits>

class Semaphore
{
public:

    Semaphore()
        : m_counter()
    {}

    void Notify()
    {
        std::unique_lock<std::mutex> lk(m_mutex);

        ++m_counter;

        m_cv.notify_one();
    }

    void Wait()
    {
        std::unique_lock<std::mutex> lk(m_mutex);

        m_cv.wait(lk, [this]() { return m_counter > 0; });

        --m_counter;
    }

    bool Wait(uint32_t waitMilliseconds)
    {
        std::unique_lock<std::mutex> lk(m_mutex);

        if (m_cv.wait_for(lk, std::chrono::milliseconds(waitMilliseconds), [this]() { return m_counter > 0; }))
        {
            --m_counter;
            return true;
        }
        else
        {
            return false;
        }
    }

private:

    std::condition_variable m_cv;

    std::mutex m_mutex;

    uint32_t m_counter;
};

template <typename T>
class WaitForResponse
    : private boost::noncopyable
{
    typedef typename boost::mpl::if_<std::is_void<T>,
        bond::Void,
        T
    >::type value_type;

    typedef typename boost::mpl::if_<std::is_void<T>,
        bond::comm::message<void>,
        bond::comm::message<value_type>
    >::type result_type;

public:
    std::function<void(const result_type&)> Callback()
    {
        BOOST_ASSERT(NULL == m_implementation);

        m_implementation = boost::make_shared<PImpl>();
        return boost::bind(&PImpl::Callback, m_implementation, _1);
    }


    operator std::function<void(const result_type&)>()
    {
        return Callback();
    }


    result_type GetResult()
    {
        return *TakeImpl()->Get(UINT_MAX);
    }


    result_type GetResult(uint32_t waitMilliseconds)
    {
        return *TakeImpl()->Get(waitMilliseconds);
    }


    void Wait()
    {
        GetResult().value();
    }


    template <typename X>
    void Deserialize(X& value)
    {
        GetResult().value().Deserialize(value);
    }

    template <typename X>
    void Deserialize(X& value, uint32_t waitMilliseconds)
    {
        GetResult(waitMilliseconds).value().Deserialize(value);
    }

private:
    class PImpl
    {
    public:
        PImpl()
        {}


        boost::shared_ptr<result_type> Get(uint32_t waitMilliseconds)
        {
            //
            // Wait for Callback.
            //
            if (!m_event.Wait(waitMilliseconds))
            {
                //
                // Wait time elapsed.
                //
                BOND_THROW(bond::comm::TransportException, "Timeout triggered");
            }

            return boost::atomic_load(&m_result);
        }


        void Callback(const result_type& msg)
        {
            BOOST_ASSERT(nullptr == m_result);

            boost::atomic_store(&m_result,
                boost::make_shared<result_type>(msg));

            m_event.Notify();
        }

    private:

        boost::shared_ptr<result_type> m_result;

        Semaphore m_event;
    };

    boost::shared_ptr<PImpl> TakeImpl()
    {
        BOOST_ASSERT(NULL != m_implementation);

        boost::shared_ptr<PImpl> temp;
        m_implementation.swap(temp);

        return temp;
    }

    boost::shared_ptr<PImpl> m_implementation;
};

const static uint32_t MAX_WAIT_IN_MSSECONDS = 1000;
const static uint64_t MSECONDS_IN_SECOND = 1000;
const static uint64_t SECONDS_IN_DAY = 3600 * 24;
const static uint64_t MSECONDS_IN_DAY = SECONDS_IN_DAY * MSECONDS_IN_SECOND;

inline uint32_t GetMilliseconds()
{
    return static_cast<uint32_t>(boost::chrono::system_clock::now().time_since_epoch().count() / 10000);
}

inline uint32_t GetMillisecondsDiff(uint32_t startMs, uint32_t endMs)
{
    uint32_t latency = (2 * MSECONDS_IN_DAY + endMs - startMs) % MSECONDS_IN_DAY;
    return (latency >= MSECONDS_IN_DAY / 2) ? 0 : latency;
}

#ifdef DEBUG
#   define LOOP(str)
#   define ONCE(str)
#else
#   define LOOP(str) for (uint32_t i = 0, t = GetMilliseconds(); i < 5 || (std::cout << str ": " << (GetMilliseconds() - t) << std::endl, false); ++i)
#   define ONCE(str) for (uint32_t i = 0, t = GetMilliseconds(); i < 1 || (std::cout << str ": " << (GetMilliseconds() - t) << std::endl, false); ++i)
#endif
