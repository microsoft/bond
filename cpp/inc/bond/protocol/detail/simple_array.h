// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/assert.hpp>
#include <boost/static_assert.hpp>

#include <cstring>
#include <limits>
#include <new>
#include <stdexcept>


namespace bond
{
namespace detail
{

template <typename T, uint32_t N = 64>
class SimpleArray
{
    BOOST_STATIC_ASSERT(std::is_pod<T>::value);
    BOOST_STATIC_ASSERT(N != 0);

public:
    SimpleArray()
        : _size(0),
          _capacity(N),
          _data(_insitu)
    {}

    ~SimpleArray()
    {
        memfree();
    }

    uint32_t size() const
    {
        return _size;
    }

    const T* begin() const
    {
        return _data;
    }

    const T& top() const
    {
        if (_size == 0)
        {
            throw std::underflow_error("Accessing empty array");
        }
        return _data[_size - 1];
    }

    const T& top(const std::nothrow_t&) const
    {
        BOOST_ASSERT(_size != 0);
        return _data[_size - 1];
    }

    T& top()
    {
        if (_size == 0)
        {
            throw std::underflow_error("Accessing empty array");
        }
        return _data[_size - 1];
    }

    T& top(const std::nothrow_t&)
    {
        BOOST_ASSERT(_size != 0);
        return _data[_size - 1];
    }

    const T& pop()
    {
        if (_size == 0)
        {
            throw std::underflow_error("Can't pop empty array");
        }
        return _data[--_size];
    }

    const T& pop(const std::nothrow_t&)
    {
        BOOST_ASSERT(_size != 0);
        return _data[--_size];
    }

    T& operator[](uint32_t i)
    {
        BOOST_ASSERT(i < _size);
        return _data[i];
    }

    void push(const T& x)
    {
        if (_size < _capacity)
            _data[_size++] = x;
        else
            grow(x);
    }

private:
    void grow(const T& x)
    {
        // cap elements to prevent overflow
        if (_capacity >= ((std::numeric_limits<uint32_t>::max)() >> 1))
        {
            throw std::bad_alloc();
        }

        T* new_data = new T[_capacity <<= 1];
        std::memcpy(new_data, _data, _size * sizeof(T));
        memfree();
        (_data = new_data)[_size++] = x;
    }

    void memfree()
    {
        if (_data != _insitu)
        {
            delete [] _data;
            _data = nullptr;
        }
    }

    uint32_t _size;
    uint32_t _capacity;
    T        _insitu[N];
    T*       _data;
};

} // namespace detail
} // namespace bond
