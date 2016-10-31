#pragma once

#include <bond/comm/message.h>

#include <memory>
#include <type_traits>

namespace bond { namespace comm
{
    namespace detail
    {
        template <typename T>
        class Continuation
        {
        public:
            Continuation(const std::function<void (const message<T>&)>& callback)
                : _callback(callback)
            {}

            template <typename Future>
            void operator()(Future&& result)
            {
                try
                {
                    _callback(result.get());
                }
                catch (const std::exception& ex)
                {
                    _callback(error(ErrorCode::INVALID_INVOCATION, ex.what()));
                }
            }
        
        private:
            std::function<void (const message<T>&)> _callback;
        };

    } // namespace detail

    template <typename T>
    detail::Continuation<T> Continuation(
        const std::function<void (const message<T>&)>& callback)
    {
        return detail::Continuation<T>(callback);
    }

    //
    // Helper function to handle future "then" asynchronously.
    //
    template <typename Future, typename Callback>
    void when(Future&& future, Callback&& callback)
    {
        struct holder
        {
            // std::decay_t<Callback>: decay_t is not supported in C++11
            typename std::decay<Callback>::type callback;

            decltype(std::declval<Future>().then(std::declval<Callback>())) result;

            explicit holder(Callback&& c)
                : callback(std::forward<Callback>(c))
            {}
        };

        std::shared_ptr<holder> ptr = std::make_shared<holder>(std::forward<Callback>(callback));

        ptr->result = std::forward<Future>(future).then([ptr](Future&& f) mutable {
            auto tmp = std::move(ptr);
            tmp->callback(std::forward<Future>(f));
        });
    }

} } // namespace bond.comm
