// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "container_interface.h"
#include "detail/alloc.h"

#include <boost/assert.hpp>
#include <boost/optional.hpp>
#include <boost/utility/enable_if.hpp>

#include <memory>
#include <stdint.h>

namespace bond
{

namespace detail
{

template <typename T> struct
use_value
    : std::integral_constant<bool,
        is_list_container<T>::value
        || is_set_container<T>::value
        || is_map_container<T>::value
        || is_string<T>::value
        || is_wstring<T>::value
        || !std::is_class<T>::value> {};

} // namespace detail


//
// Nullable value
//
template <
    typename T,
    typename Allocator = typename detail::allocator_type<T>::type,
    typename Enable = void>
class nullable;

#if defined(_MSC_VER) && _MSC_VER < 1900
#pragma warning(push)
#pragma warning(disable: 4510)
#endif
template <typename T, typename Allocator>
class nullable<T, Allocator, typename boost::enable_if<detail::use_value<T> >::type>
    : private detail::allocator_holder<Allocator>
{
    using alloc_holder = detail::allocator_holder<Allocator>;

public:
    using value_type = T;
    using allocator_type = Allocator;

    nullable() = default;

    explicit
    nullable(const allocator_type& alloc)
        : alloc_holder(alloc)
    {}

    nullable(const nullable& other) = default;

    nullable(const nullable& other, const allocator_type& alloc)
        : alloc_holder(alloc)
    {
        if (other.hasvalue())
        {
            _value.emplace(*other._value, alloc);
        }
    }

    nullable(nullable&& other) BOND_NOEXCEPT_IF(
            std::is_nothrow_move_constructible<alloc_holder>::value
            && std::is_nothrow_move_constructible<T>::value)
        : alloc_holder(std::move(other)),
          _value(std::move(other._value))
    {
        other._value = boost::none; // assigning boost::none is noexcept
    }

    nullable(nullable&& other, const allocator_type& alloc)
        : alloc_holder(alloc),
          _value(std::move(other._value), alloc)
    {
        other._value = boost::none; // assigning boost::none is noexcept
    }

    explicit
    nullable(const T& value)
        : alloc_holder(detail::get_allocator(value)),
          _value(value)
    {}

    explicit
    nullable(T&& value)
        : alloc_holder(detail::get_allocator(value)),
          _value(std::move(value))
    {}

    nullable& operator=(const nullable& src) = default;

    nullable& operator=(nullable&& other) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<alloc_holder>::value
        && std::is_nothrow_move_constructible<T>::value)
    {
        alloc_holder::operator=(std::move(other));
        _value = std::move(other._value);
        other._value = boost::none; // assigning boost::none is noexcept
        return *this;
    }

    bool hasvalue() const BOND_NOEXCEPT
    {
        return static_cast<bool>(_value);
    }

    bool empty() const BOND_NOEXCEPT
    {
        return !hasvalue();
    }

    explicit operator bool() const BOND_NOEXCEPT
    {
        return !empty();
    }

    T& value() BOND_NOEXCEPT
    {
        BOOST_ASSERT(hasvalue());
        return *_value;
    }

    const T& value() const BOND_NOEXCEPT
    {
        BOOST_ASSERT(hasvalue());
        return *_value;
    }

    T& operator*() BOND_NOEXCEPT
    {
        return value();
    }

    const T& operator*() const BOND_NOEXCEPT
    {
        return value();
    }

    T* operator->() BOND_NOEXCEPT
    {
        return &value();
    }

    const T* operator->() const BOND_NOEXCEPT
    {
        return &value();
    }

    T& set()
    {
        if (empty())
            set_value();

        return *_value;
    }

    void set(const T& value) BOND_NOEXCEPT_IF(std::is_nothrow_copy_constructible<T>::value)
    {
        _value = value;
    }

    void set(T&& value) BOND_NOEXCEPT_IF(std::is_nothrow_move_constructible<T>::value)
    {
        _value = std::move(value);
    }

    void reset() BOND_NOEXCEPT
    {
        _value = boost::none;
    }

    void clear() BOND_NOEXCEPT
    {
        reset();
    }

    void swap(nullable& other)
    {
        using std::swap;
        swap(static_cast<alloc_holder&>(*this), static_cast<alloc_holder&>(other));
        swap(_value, other._value);
    }

    allocator_type get_allocator() const BOND_NOEXCEPT
    {
        return alloc_holder::get();
    }

private:
    template <typename U = T>
    typename boost::enable_if<detail::has_allocator<U> >::type
    set_value()
    {
        _value.emplace(alloc_holder::get());
    }

    template <typename U = T>
    typename boost::disable_if<detail::has_allocator<U> >::type
    set_value()
    {
        _value.emplace();
    }

    boost::optional<T> _value;
};
#if defined(_MSC_VER) && _MSC_VER < 1900
#pragma warning(pop)
#endif


/** @brief Nullable type */
/** See [User's Manual](../../manual/bond_cpp.html#nullable-types) */
template <typename T, typename Allocator>
class nullable<T, Allocator, typename boost::disable_if<detail::use_value<T> >::type>
    : private detail::allocator_holder<Allocator>
{
    using alloc_holder = detail::allocator_holder<Allocator>;
    using rebind_alloc = typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
    using pointer = typename std::allocator_traits<rebind_alloc>::pointer;

public:
    using value_type = T;
    using allocator_type = Allocator;

    /// @brief Default constructor
    nullable() BOND_NOEXCEPT_IF(
        std::is_nothrow_default_constructible<alloc_holder>::value
        && std::is_nothrow_default_constructible<pointer>::value)
        : alloc_holder(),
          _value()
    {}

    /// @brief Construct nullable using specified allocator instance
    explicit
    nullable(const allocator_type& alloc)
        : alloc_holder(alloc),
          _value()
    {}

    /// @brief Copy constructor
    nullable(const nullable& other)
        : alloc_holder(other),
          _value(other.hasvalue() ? new_value(other.value()) : pointer())
    {}

    nullable(const nullable& other, const allocator_type& alloc)
        : alloc_holder(alloc),
          _value(other.hasvalue() ? new_value(other.value(), alloc) : pointer())
    {}

    nullable(nullable&& other) BOND_NOEXCEPT_IF(
            std::is_nothrow_move_constructible<alloc_holder>::value
            && std::is_nothrow_move_constructible<pointer>::value
            && BOND_NOEXCEPT(other._value = {}))
        : alloc_holder(std::move(other)),
          _value(std::move(other._value))
    {
        other._value = {};
    }

    nullable(nullable&& other, const allocator_type& alloc)
        : alloc_holder(alloc),
          _value(other.alloc_holder::get() == alloc
              ? std::move(other._value)
              : (other.hasvalue() ? new_value(std::move(*other._value), alloc) : pointer()))
    {
        other._value = {};
    }

    /// @brief Construct from an instance T
    explicit
    nullable(const T& value, const allocator_type& alloc = {})
        : alloc_holder(alloc),
          _value(new_value(value))
    {}

    explicit
    nullable(T&& value, const allocator_type& alloc = {})
        : alloc_holder(alloc),
          _value(new_value(std::move(value)))
    {}

    /// @brief Assignment operator
    nullable& operator=(const nullable& other)
    {
        nullable(other).swap(*this);
        return *this;
    }

    nullable& operator=(nullable&& other)
    {
        nullable(std::move(other)).swap(*this);
        return *this;
    }

    ~nullable()
    {
        reset();
    }

    bool hasvalue() const BOND_NOEXCEPT
    {
        return !!_value;
    }

    /// @brief Checks if the object is null
    bool empty() const BOND_NOEXCEPT
    {
        return !hasvalue();
    }

    explicit operator bool() const BOND_NOEXCEPT
    {
        return !empty();
    }

    /// @brief Return reference to contained value
    ///
    /// Undefined if the object is null
    T& value() BOND_NOEXCEPT
    {
        BOOST_ASSERT(hasvalue());
        return *_value;
    }

    /// @brief Return constant reference to contained value
    ///
    /// Undefined if the object is null
    const T& value() const BOND_NOEXCEPT
    {
        BOOST_ASSERT(hasvalue());
        return *_value;
    }

    /// @brief Dereference operator
    ///
    /// Undefined if the object is null
    T& operator*() BOND_NOEXCEPT
    {
        return value();
    }

    /// @brief Dereference operator
    ///
    /// Undefined if the object is null
    const T& operator*() const BOND_NOEXCEPT
    {
        return value();
    }

    T* operator->() BOND_NOEXCEPT
    {
        return &value();
    }

    const T* operator->() const BOND_NOEXCEPT
    {
        return &value();
    }

    /// @brief Set to default instance of T and return reference to the value
    T& set()
    {
        if (empty())
            _value = set_value();

        return *_value;
    }

    /// @brief Set to specified value
    void set(const T& value)
    {
        set_value(value);
    }

    void set(T&& value)
    {
        set_value(std::move(value));
    }

    /// @brief Reset to null
    void reset()
    {
        if (hasvalue())
        {
            delete_value();
            _value = {};
        }
    }

    /// @brief The same as reset
    void clear()
    {
        reset();
    }

    void swap(nullable& other)
    {
        using std::swap;
        swap(static_cast<alloc_holder&>(*this), static_cast<alloc_holder&>(other));
        swap(_value, other._value);
    }

    allocator_type get_allocator() const BOND_NOEXCEPT
    {
        return alloc_holder::get();
    }

private:
    void delete_value()
    {
        rebind_alloc alloc(alloc_holder::get());
        std::allocator_traits<rebind_alloc>::destroy(alloc, std::addressof(*_value));
        alloc.deallocate(_value, 1);
    }

    template <typename... Args>
    pointer new_value(Args&&... args)
    {
        rebind_alloc alloc(alloc_holder::get());
        pointer p(alloc.allocate(1));
        try
        {
            std::allocator_traits<rebind_alloc>::construct(
                alloc, std::addressof(*p), std::forward<Args>(args)...);
            return p;
        }
        catch (...)
        {
            alloc.deallocate(p, 1);
            throw;
        }
    }

    template <typename U = T>
    typename boost::enable_if<detail::has_allocator<U>, pointer>::type
    set_value()
    {
        return new_value(alloc_holder::get());
    }

    template <typename U = T>
    typename boost::disable_if<detail::has_allocator<U>, pointer>::type
    set_value()
    {
        return new_value();
    }

    template <typename U>
    void set_value(U&& value)
    {
        if (empty())
            _value = new_value(std::forward<U>(value));
        else
            *_value = std::forward<U>(value);
    }

    pointer _value;
};


template <typename T, typename Allocator>
inline void swap(nullable<T, Allocator>& x, nullable<T, Allocator>& y)
{
    x.swap(y);
}


template <typename T, typename Allocator>
inline bool operator==(const nullable<T, Allocator>& x, const nullable<T, Allocator>& y)
{
    return (x.hasvalue() == y.hasvalue() && (!x.hasvalue() || *x == *y));
}


template <typename T, typename Allocator>
inline bool operator!=(const nullable<T, Allocator>& x, const nullable<T, Allocator>& y)
{
    return !(x == y);
}


// nullable<T> is internally treated as a list container with 0 or 1 element

// container_size
template <typename T, typename Allocator>
uint32_t container_size(const nullable<T, Allocator>& value)
{
    return value.empty() ? 0 : 1;
}


// resize_list
template <typename T, typename Allocator>
void resize_list(nullable<T, Allocator>& value, uint32_t size)
{
    if (size)
        value.set();
    else
        value.reset();
}


template <typename T, typename Allocator> struct
element_type<nullable<T, Allocator> >
{
    typedef T type;
};


// enumerators
template <typename T, typename Allocator>
class const_enumerator<nullable<T, Allocator> >
{
public:
    const_enumerator(const nullable<T, Allocator>& value)
        : _value(value),
          _more(value.hasvalue())
    {}

    const_enumerator(const const_enumerator& other) = delete;
    const_enumerator& operator=(const const_enumerator& other) = delete;

    bool more()
    {
        return _more;
    }

    const T& next()
    {
        _more = false;
        return _value.value();
    }

private:
    const nullable<T, Allocator>& _value;
    bool _more;
};


template <typename T, typename Allocator>
class enumerator<nullable<T, Allocator> >
{
public:
    enumerator(nullable<T, Allocator>& value)
        : _value(value),
          _more(value.hasvalue())
    {}

    enumerator(const enumerator& other) = delete;
    enumerator& operator=(const enumerator& other) = delete;

    bool more()
    {
        return _more;
    }

    T& next()
    {
        _more = false;
        return _value.value();
    }

private:
    nullable<T, Allocator>& _value;
    bool _more;
};


template <typename T, typename Allocator> struct
is_list_container<nullable<T, Allocator> >
    : std::true_type {};


} // namespace bond
