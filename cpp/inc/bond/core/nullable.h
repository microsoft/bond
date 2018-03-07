// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "container_interface.h"

#include <boost/assert.hpp>
#include <boost/noncopyable.hpp>
#include <boost/static_assert.hpp>
#include <boost/utility/addressof.hpp>
#include <boost/utility/enable_if.hpp>

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

template<typename T, typename E = void> struct
has_allocator
    : std::false_type {};

template<typename T> struct
has_allocator<T, typename boost::enable_if<std::is_class<typename T::allocator_type> >::type>
    : std::true_type {};

template<typename T, typename E = void> struct
allocator_type
{
    typedef std::allocator<T> type;
};

template<typename T> struct
allocator_type<T, typename boost::enable_if<has_allocator<T> >::type>
{
    typedef typename T::allocator_type type;
};

template<typename T>
typename boost::enable_if<has_allocator<T>, typename allocator_type<T>::type>::type
get_allocator(const T& value)
{
    return value.get_allocator();
}

template<typename T>
typename boost::disable_if<has_allocator<T>, typename allocator_type<T>::type>::type
get_allocator(const T&)
{
    return typename allocator_type<T>::type();
}

} // namespace detail


//
// Nullable value
//
template<typename T,
         typename Allocator = typename detail::allocator_type<T>::type,
         bool useValue = detail::use_value<T>::value>
class nullable;

template<typename T, typename Allocator>
class nullable<T, Allocator, true>
    : Allocator
{
public:
    typedef T           value_type;
    typedef T*          pointer;
    typedef const T*    const_pointer;
    typedef T&          reference;
    typedef const T&    const_reference;
    typedef Allocator   allocator_type;

    bool hasvalue() const
    {
        return _hasvalue;
    }

    bool empty() const
    {
        return !hasvalue();
    }

    void swap(nullable& src)
    {
        std::swap(base(), src.base());
        std::swap(_hasvalue, src._hasvalue);
        std::swap(_value, src._value);
    }

    nullable()
        : _value(),
          _hasvalue(false)
    {}

    explicit
    nullable(const allocator_type& alloc)
        : Allocator(alloc),
          _value(make_value<value_type>()),
          _hasvalue(false)
    {
    }

    explicit
    nullable(const value_type& value)
        : Allocator(detail::get_allocator(value)),
          _value(value),
          _hasvalue(true)
    {}

    nullable(const nullable& src)
        : Allocator(src.base()),
          _value(src._value),
          _hasvalue(src._hasvalue)
    {}

    nullable(const nullable& src, const allocator_type& alloc)
        : Allocator(alloc),
          _value(src._value, alloc),
          _hasvalue(src._hasvalue)
    {}

    nullable& operator=(const nullable& src)
    {
        nullable(src).swap(*this);
        return *this;
    }

    reference value()
    {
        BOOST_ASSERT(hasvalue());
        return _value;
    }

    const_reference value() const
    {
        BOOST_ASSERT(hasvalue());
        return _value;
    }

    reference operator*()
    {
        return value();
    }

    const_reference operator*() const
    {
        return value();
    }

    pointer operator->()
    {
        return &value();
    }

    const_pointer operator->() const
    {
        return &value();
    }

    bool operator!() const
    {
        return empty();
    }

    reference set()
    {
        _hasvalue = true;
        return _value;
    }

    void set(const_reference value)
    {
        _value = value;
        _hasvalue = true;
    }

    void reset()
    {
        _value = make_value<value_type>();
        _hasvalue = false;
    }

    void clear()
    {
        reset();
    }

    allocator_type get_allocator() const
    {
        return base();
    }

    explicit
    nullable(value_type&& value) BOND_NOEXCEPT_IF(
            std::is_nothrow_move_constructible<Allocator>::value
            && std::is_nothrow_move_constructible<value_type>::value)
        : Allocator(detail::get_allocator(value)),
          _value(std::move(value)),
          _hasvalue(true)
    {}

    nullable(nullable&& src) BOND_NOEXCEPT_IF(
            std::is_nothrow_move_constructible<Allocator>::value
            && std::is_nothrow_move_constructible<value_type>::value)
        : Allocator(std::move(src.base())),
          _value(std::move(src._value)),
          _hasvalue(std::move(src._hasvalue))
    {
        src._hasvalue = false;
    }

    nullable(nullable&& src, const allocator_type& alloc)
        : Allocator(alloc),
          _value(std::move(src._value), alloc),
          _hasvalue(std::move(src._hasvalue))
    {
        src._hasvalue = false;
    }

    nullable& operator=(nullable&& src) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<Allocator>::value
        && std::is_nothrow_move_constructible<value_type>::value)
    {
        if (this != &src)
        {
            base() = std::move(src.base());
            _value = std::move(src._value);
            _hasvalue = std::move(src._hasvalue);
            src._hasvalue = false;
        }
        return *this;
    }

    void set(value_type&& value) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<value_type>::value)
    {
        _value = std::move(value);
        _hasvalue = true;
    }

private:
    Allocator& base()
    {
        return static_cast<allocator_type&>(*this);
    }

    const Allocator& base() const
    {
        return static_cast<const allocator_type&>(*this);
    }

    template<typename ValueType>
    typename boost::enable_if<detail::has_allocator<ValueType>, ValueType>::type
    make_value()
    {
        return ValueType(base());
    }

    template<typename ValueType>
    typename boost::disable_if<detail::has_allocator<ValueType>, ValueType>::type
    make_value()
    {
        return ValueType();
    }

private:
    value_type  _value;
    bool        _hasvalue;
};


/** @brief Nullable type */
/** See [User's Manual](../../manual/bond_cpp.html#nullable-types) */
template<typename T, typename Allocator>
class nullable<T, Allocator, false>
    : Allocator
{
    BOOST_STATIC_ASSERT(!detail::use_value<T>::value);

public:
    typedef T           value_type;
    typedef T*          pointer;
    typedef const T*    const_pointer;
    typedef T&          reference;
    typedef const T&    const_reference;
    typedef Allocator   allocator_type;

private:
    typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<value_type> rebind_alloc;

    typedef typename std::allocator_traits<rebind_alloc>::pointer real_pointer;

public:
    bool hasvalue() const
    {
        return !!_value;
    }

    /// @brief Checks if the object is null
    bool empty() const
    {
        return !hasvalue();
    }

    void swap(nullable& src)
    {
        std::swap(base(), src.base());
        std::swap(_value, src._value);
    }

    /// @brief Default constructor
    nullable()
        : _value()
    {}

    /// @brief Construct nullable using specified allocator instance
    explicit
    nullable(const allocator_type& alloc)
        : Allocator(alloc),
          _value()
    {}

    /// @brief Construct from an instance T
    explicit
    nullable(const value_type& value,
             const allocator_type& alloc = allocator_type())
        : Allocator(alloc),
          _value(new_value(value))
    {}

    /// @brief Copy constructor
    nullable(const nullable& src)
        : Allocator(src.base()),
          _value(src.hasvalue() ? new_value(src.value()) : real_pointer())
    {}

    nullable(const nullable& src, const allocator_type& alloc)
        : Allocator(alloc),
          _value(src.hasvalue() ? new_value(src.value(), alloc) : real_pointer())
    {}

    ~nullable()
    {
        reset();
    }

    /// @brief Assignment operator
    nullable& operator=(const nullable& src)
    {
        nullable(src).swap(*this);
        return *this;
    }

    /// @brief Return reference to contained value
    ///
    /// Undefined if the object is null
    reference value()
    {
        BOOST_ASSERT(hasvalue());
        return *_value;
    }

    /// @brief Return constant reference to contained value
    ///
    /// Undefined if the object is null
    const_reference value() const
    {
        BOOST_ASSERT(hasvalue());
        return *_value;
    }

    /// @brief Dereference operator
    ///
    /// Undefined if the object is null
    reference operator*()
    {
        return value();
    }

    /// @brief Dereference operator
    ///
    /// Undefined if the object is null
    const_reference operator*() const
    {
        return value();
    }

    pointer operator->()
    {
        return &value();
    }

    const_pointer operator->() const
    {
        return &value();
    }

    bool operator!() const
    {
        return empty();
    }

    /// @brief Set to default instance of T and return reference to the value
    reference set()
    {
        if (empty())
            _value = set_value(detail::has_allocator<T>());
        return *_value;
    }

    /// @brief Set to specified value
    void set(const_reference value)
    {
        if (empty())
            _value = new_value(value);
        else
            *_value = value;
    }

    /// @brief Reset to null
    void reset()
    {
        if (_value)
        {
            delete_value();
            _value = real_pointer();
        }
    }

    /// @brief The same as reset
    void clear()
    {
        reset();
    }

    allocator_type get_allocator() const
    {
        return base();
    }

    explicit
    nullable(value_type&& value,
             const allocator_type& alloc = allocator_type())
        : Allocator(alloc),
          _value(new_value(std::move(value)))
    {}

    nullable(nullable&& src) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<Allocator>::value
        && std::is_nothrow_move_constructible<real_pointer>::value
        && BOND_NOEXCEPT(src._value = real_pointer()))
        : Allocator(std::move(src.base())),
          _value(std::move(src._value))
    {
        src._value = real_pointer();
    }

    nullable(nullable&& src, const allocator_type& alloc)
        : Allocator(alloc),
          _value(src.base() == alloc
              ? std::move(src._value)
              : (src.hasvalue() ? new_value(std::move(*src._value), alloc) : real_pointer()))
    {
        src._value = real_pointer();
    }

    nullable& operator=(nullable&& src)
    {
        nullable(std::move(src)).swap(*this);
        return *this;
    }

    void set(value_type&& value)
    {
        if (empty())
            _value = new_value(std::move(value));
        else
            *_value = std::move(value);
    }

private:
    Allocator& base()
    {
        return static_cast<allocator_type&>(*this);
    }

    const Allocator& base() const
    {
        return static_cast<const allocator_type&>(*this);
    }

    void delete_value()
    {
        rebind_alloc alloc(base());
        std::allocator_traits<rebind_alloc>::destroy(alloc, boost::addressof(*_value));
        alloc.deallocate(_value, 1);
    }

    template<typename... Args>
    real_pointer new_value(Args&&... args)
    {
        rebind_alloc alloc(base());
        real_pointer p(alloc.allocate(1));
        try
        {
            std::allocator_traits<rebind_alloc>::construct(alloc,
                boost::addressof(*p),
                std::forward<Args>(args)...);
            return p;
        }
        catch (...)
        {
            alloc.deallocate(p, 1);
            throw;
        }
    }

    real_pointer new_value()
    {
        rebind_alloc alloc(base());
        real_pointer p(alloc.allocate(1));
        try
        {
            std::allocator_traits<rebind_alloc>::construct(alloc,
                boost::addressof(*p));
            return p;
        }
        catch (...)
        {
            alloc.deallocate(p, 1);
            throw;
        }
    }

    real_pointer set_value(std::true_type)
    {
        return new_value(base());
    }

    real_pointer set_value(std::false_type)
    {
        return new_value();
    }

private:
    real_pointer _value;
};


template<typename T, typename Allocator, bool useValue>
inline void swap(nullable<T, Allocator, useValue>& x,
                 nullable<T, Allocator, useValue>& y)
{
    x.swap(y);
}


template<typename T, typename Allocator, bool useValue>
inline bool operator==(const nullable<T, Allocator, useValue>& x,
                       const nullable<T, Allocator, useValue>& y)
{
    return (x.hasvalue() == y.hasvalue()
        && (!x.hasvalue() || *x == *y));
}


template<typename T, typename Allocator, bool useValue>
inline bool operator!= (const nullable<T, Allocator, useValue>& x,
                        const nullable<T, Allocator, useValue>& y)
{
    return !(x == y);
}


// nullable<T> is internally treated as a list container with 0 or 1 element

// container_size
template <typename T, typename Allocator, bool useValue>
uint32_t container_size(const nullable<T, Allocator, useValue>& value)
{
    return value.empty() ? 0 : 1;
}


// resize_list
template <typename T, typename Allocator, bool useValue>
void resize_list(nullable<T, Allocator, useValue>& value, uint32_t size)
{
    if (size)
        value.set();
    else
        value.reset();
}


template <typename T, typename Allocator, bool useValue> struct
element_type<nullable<T, Allocator, useValue> >
{
    typedef T type;
};


// enumerators
template <typename T, typename Allocator, bool useValue>
class const_enumerator<nullable<T, Allocator, useValue> >
    : boost::noncopyable
{
public:
    const_enumerator(const nullable<T, Allocator, useValue>& value)
        : _value(value),
          _more(value.hasvalue())
    {}

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
    const nullable<T, Allocator, useValue>& _value;
    bool                         _more;
};


template <typename T, typename Allocator, bool useValue>
class enumerator<nullable<T, Allocator, useValue> >
    : boost::noncopyable
{
public:
    enumerator(nullable<T, Allocator, useValue>& value)
        : _value(value),
          _more(value.hasvalue())
    {}

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
    nullable<T, Allocator, useValue>&  _value;
    bool _more;
};


template <typename T, typename Allocator, bool useValue> struct
is_list_container<nullable<T, Allocator, useValue> >
    : std::true_type {};


} // namespace bond
