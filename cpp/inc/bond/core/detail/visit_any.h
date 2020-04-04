// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "mpl.h"

#include <boost/optional.hpp>

#include <type_traits>

namespace bond
{

namespace detail
{

#if !defined(BOND_NO_CXX14_RETURN_TYPE_DEDUCTION) && !defined(BOND_NO_CXX14_GENERIC_LAMBDAS)

#if defined(BOND_CXX_17)
    template<typename Visitor, typename T>
    using invoke_result_of_t = std::invoke_result_t<Visitor, T&>;
#else
    template<typename Visitor, typename T>
    using invoke_result_of_t = std::result_of_t<Visitor(T&)>;
#endif

template <typename T, typename Visitor, typename Any>
inline typename boost::disable_if<std::is_void<invoke_result_of_t<Visitor, T> >, boost::optional<invoke_result_of_t<Visitor, T> > >::type
try_visit_any(Visitor&& visitor, Any& x)
{
    if (auto value = any_cast<T>(&x))
    {
        return std::forward<Visitor>(visitor)(*value);
    }

    return {};
}

template <typename T, typename Visitor, typename Any>
inline typename boost::enable_if<std::is_void<invoke_result_of_t<Visitor, T> >, bool>::type
try_visit_any(Visitor&& visitor, Any& x)
{
    if (auto value = any_cast<T>(&x))
    {
        std::forward<Visitor>(visitor)(*value);
        return true;
    }

    return false;
}

template <typename List, typename Visitor, typename Any>
inline auto visit_any(Visitor&& visitor, Any& x)
{
    return mpl::try_apply<List>(
        [&](auto identity)
        {
            (void)identity;
            return try_visit_any<typename decltype(identity)::type>(std::forward<Visitor>(visitor), x);
        });
}

#else

template <typename Result, typename Visitor, typename Any>
struct try_visit_any
{
    try_visit_any(Visitor& visitor, Any& x)
        : _visitor(visitor),
          _x(x)
    {}

    try_visit_any& operator=(const try_visit_any& other) = delete;

    template <typename T, typename R = Result>
    typename boost::disable_if<std::is_void<R>, boost::optional<Result> >::type
    operator()(const mpl::identity<T>&) const
    {
        if (auto value = any_cast<T>(&_x))
        {
            return _visitor(*value);
        }

        return {};
    }

    template <typename T, typename R = Result>
    typename boost::enable_if<std::is_void<R>, bool>::type
    operator()(const mpl::identity<T>&) const
    {
        if (auto value = any_cast<T>(&_x))
        {
            _visitor(*value);
            return true;
        }

        return false;
    }

    Visitor& _visitor;
    Any& _x;
};

template <typename T> struct
visitor_result
{
    using type = boost::optional<T>;
};

template <> struct
visitor_result<void>
{
    using type = bool;
};

template <typename Result, typename Visitor, typename Any, typename T, typename... U>
inline typename visitor_result<Result>::type visit_any(Visitor&& visitor, Any& x, const mpl::list<T, U...>&)
{
    return mpl::try_apply<mpl::list<T, U...> >(try_visit_any<Result, Visitor, Any>(visitor, x));
}

template <typename List, typename Result, typename Visitor, typename Any>
inline typename visitor_result<Result>::type visit_any(Visitor&& visitor, Any& x)
{
    return visit_any<Result>(std::forward<Visitor>(visitor), x, List{});
}

#endif


} // namespace detail

} // namespace bond
