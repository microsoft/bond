#pragma once

#include <boost/static_assert.hpp>
#include <type_traits>


namespace bond
{

namespace detail {namespace mpl
{

template <typename T>
struct identity
{
    using type = T;
};


/// @brief Represents a type list.
template <typename... T>
struct list
{};


/// @brief Appends the given list of types or a single pack of list<> to the end.
template <typename List, typename... T>
struct append;

template <typename List, typename... T>
using append_t = typename append<List, T...>::type;

template <typename List, typename... T>
struct append : append<List, list<T...> >
{};

template <typename... T, typename... U>
struct append<list<T...>, list<U...> > : identity<list<T..., U...> >
{};


/// @brief Filters the given type list with the provided predicate.
template <typename List, template <typename> class C>
struct filter;

template <typename List, template <typename> class C>
using filter_t = typename filter<List, C>::type;

template <template <typename> class C>
struct filter<list<>, C> : identity<list<> >
{};

template <typename T, typename... U, template <typename> class C>
struct filter<list<T, U...>, C> : append<
    typename std::conditional<C<T>::value, list<T>, list<> >::type,
    filter_t<list<U...>, C> >
{};


template <typename F, typename... T>
void apply(F&& f, const list<T...>&)
{
    std::initializer_list<int>{ (f(identity<T>{}), 0)... };
}

template <typename F>
void apply(F&& /*f*/, const list<>&)
{}

template <typename List, typename F>
void apply(F&& f)
{
    apply(std::forward<F>(f), List{});
}


template <typename F, typename T>
auto try_apply(F&& f, const list<T>&)
#ifdef BOND_NO_CXX14_RETURN_TYPE_DEDUCTION
    -> decltype(std::forward<F>(f)(identity<T>{}))
#endif
{
    return std::forward<F>(f)(identity<T>{});
}

template <typename F, typename T, typename U, typename... R>
auto try_apply(F&& f, const list<T, U, R...>&)
#ifdef BOND_NO_CXX14_RETURN_TYPE_DEDUCTION
    -> decltype(f(identity<T>{}))
#endif
{
    if (auto&& result = f(identity<T>{}))
    {
        return result;
    }

    return try_apply(std::forward<F>(f), list<U, R...>{});
}

template <typename List, typename F>
auto try_apply(F&& f)
#ifdef BOND_NO_CXX14_RETURN_TYPE_DEDUCTION
    -> decltype(try_apply(std::forward<F>(f), List{}))
#endif
{
    BOOST_STATIC_ASSERT(!std::is_same<List, list<> >::value);

    return try_apply(std::forward<F>(f), List{});
}


}} // namespace mpl {namespace detail

} // namespace bond
