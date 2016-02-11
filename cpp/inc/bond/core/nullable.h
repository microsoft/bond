// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "config.h"
#include "container_interface.h"
#include <stdint.h>
#include <boost/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/noncopyable.hpp>
#include <boost/utility/enable_if.hpp>

namespace bond
{

namespace detail
{

template <typename T> struct
use_value
{
    static const bool value = is_list_container<T>::value
                           || is_set_container<T>::value
                           || is_map_container<T>::value
                           || is_string<T>::value
                           || is_wstring<T>::value
                           || !is_class<T>::value;
};

template<typename T, typename E = void> struct
has_compare
    : false_type {};

template<typename T> struct
has_compare<T, typename boost::enable_if<is_class<typename T::key_compare> >::type>
    : true_type {};

template<typename T, typename E = void> struct
has_allocator
    : false_type {};

template<typename T> struct
has_allocator<T, typename boost::enable_if<is_class<typename T::allocator_type> >::type>
    : true_type {};

struct no_allocator
{
    template<typename T>
    struct rebind
    {
        typedef no_allocator other;
    };
};

template<typename T, typename E = void> struct
allocator_type
{
    typedef no_allocator type;
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
        std::swap(_alloc, src._alloc);
        std::swap(_hasvalue, src._hasvalue);
        std::swap(_value, src._value);
    }

    nullable()
        : _value(),
          _hasvalue(false)
    {}

    explicit
    nullable(const allocator_type& alloc)
        : _alloc(alloc),
          _value(make_value<value_type>()),
          _hasvalue(false)
    {
    }

    // deprecated!
    template <typename Compare>
    explicit
    nullable(const Compare&,
             const allocator_type& alloc)
        : _alloc(alloc),
          _value(make_value<value_type>()),
          _hasvalue(false)
    {}

    explicit
    nullable(const value_type& value)
        : _alloc(detail::get_allocator(value)),
          _value(value),
          _hasvalue(true)
    {}

    nullable(const nullable& src)
        : _alloc(src._alloc),
          _value(src._value),
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
        return _alloc;
    }

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    explicit
    nullable(value_type&& value)
        : _alloc(detail::get_allocator(value)),
          _value(std::move(value)),
          _hasvalue(true)
    {}

    nullable(nullable&& src)
        : _alloc(std::move(src._alloc)),
          _value(std::move(src._value)),
          _hasvalue(std::move(src._hasvalue))
    {
        src._hasvalue = false;
    }

    nullable& operator=(nullable&& src)
    {
        if (this != &src)
        {
            _alloc = std::move(src._alloc);
            _value = std::move(src._value);
            _hasvalue = std::move(src._hasvalue);
            src._hasvalue = false;
        }
        return *this;
    }

    void set(value_type&& value)
    {
        _value = std::move(value);
        _hasvalue = true;
    }
#endif

private:
    template<typename ValueType>
    typename boost::enable_if_c<detail::has_allocator<ValueType>::value &&
                                detail::has_compare<ValueType>::value, ValueType>::type
    make_value()
    {
        ValueType value(typename ValueType::key_compare(), _alloc);
        return value;
    }

    template<typename ValueType>
    typename boost::enable_if_c<detail::has_allocator<ValueType>::value &&
                                !detail::has_compare<ValueType>::value, ValueType>::type
    make_value()
    {
        ValueType value(_alloc);
        return value;
    }

    template<typename ValueType>
    typename boost::disable_if<detail::has_allocator<ValueType>, ValueType>::type
    make_value()
    {
        ValueType value = ValueType();
        return value;
    }

private:
    allocator_type _alloc;
    value_type  _value;
    bool        _hasvalue;
};


/** @brief Nullable type */
/** See [User's Manual](../../manual/bond_cpp.html#nullable-types) */
template<typename T, typename Allocator>
class nullable<T, Allocator, false>
{
    BOOST_STATIC_ASSERT(!detail::use_value<T>::value);

public:
    typedef T           value_type;
    typedef T*          pointer;
    typedef const T*    const_pointer;
    typedef T&          reference;
    typedef const T&    const_reference;
    typedef Allocator   allocator_type;

    bool hasvalue() const
    {
        return _value != 0;
    }

    /// @brief Checks if the object is null
    bool empty() const
    {
        return !hasvalue();
    }

    void swap(nullable& src)
    {
        std::swap(_alloc, src._alloc);
        std::swap(_value, src._value);
    }

    /// @brief Default constructor
    nullable()
        : _value(0)
    {}

    /// @brief Construct nullable using specified allocator instance
    explicit
    nullable(const allocator_type& alloc)
        : _alloc(alloc),
          _value(0)
    {}

    /// @brief Construct from an instance T
    explicit
    nullable(const value_type& value,
             const allocator_type& alloc = allocator_type())
        : _alloc(alloc),
          _value(new_value(_alloc, value))
    {}

    /// @brief Copy constructor
    nullable(const nullable& src)
        : _alloc(src._alloc),
          _value(src.hasvalue() ? new_value(_alloc, src.value()) : 0)
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
            _value = new_value(_alloc);
        return *_value;
    }

    /// @brief Set to specified value
    void set(const_reference value)
    {
        if (empty())
            _value = new_value(_alloc, value);
        else
            *_value = value;
    }

    /// @brief Reset to null
    void reset()
    {
        if (_value)
        {
            destroy(_alloc);
            _value = 0;
        }
    }

    /// @brief The same as reset
    void clear()
    {
        reset();
    }

    allocator_type get_allocator() const
    {
        return _alloc;
    }

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    explicit
    nullable(value_type&& value,
             const allocator_type& alloc = allocator_type())
        : _alloc(alloc),
          _value(new_value(_alloc, std::move(value)))
    {}

    nullable(nullable&& src)
        : _alloc(std::move(src._alloc)),
          _value(std::move(src._value))
    {
        src._value = 0;
    }

    nullable& operator=(nullable&& src)
    {
        nullable(std::move(src)).swap(*this);
        return *this;
    }

    void set(value_type&& value)
    {
        if (empty())
            _value = new_value(_alloc, std::move(value));
        else
            *_value = std::move(value);
    }
#endif

private:
    template<typename AllocatorT>
    void destroy(AllocatorT& alloc)
    {
        alloc.destroy(_value);
        alloc.deallocate(_value, 1);
    }

    void destroy(detail::no_allocator&)
    {
        delete _value;
    }

    template<typename AllocatorT, typename Arg1>
#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    pointer new_value(AllocatorT& alloc, Arg1&& arg1)
#else
    pointer new_value(AllocatorT& alloc, const Arg1& arg1)
#endif
    {
        T* p = alloc.allocate(1);
        try
        {
            void* p1 = p;
            return ::new(p1) T(arg1);
        }
        catch (...)
        {
            alloc.deallocate(p, 1);
            throw;
        }
    }

    template<typename AllocatorT>
    pointer new_value(AllocatorT& alloc)
    {
        return new_value(alloc, alloc);
    }

    template<typename Arg1>
#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    pointer new_value(detail::no_allocator&, Arg1&& arg1)
#else
    pointer new_value(detail::no_allocator&, const Arg1& arg1)
#endif
    {
        return new T(arg1);
    }

    pointer new_value(detail::no_allocator&)
    {
        return new T();
    }

private:
    typename allocator_type::template rebind<value_type>::other _alloc;
    pointer _value;
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
    : true_type {};


} // namespace bond
