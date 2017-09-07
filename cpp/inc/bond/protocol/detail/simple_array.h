// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <limits>
#include <stdexcept>

namespace bond
{
namespace detail
{

template <typename T, uint32_t N = 64>
class SimpleArray
{
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

    T pop()
    {
        return _data[--_size];
    }

    T& operator[](uint32_t i)
    {
        return _data[i];
    }

    void push(T x)
    {
        if (_size < _capacity)
            _data[_size++] = x;
        else
            grow(x);
    }

private:
    void grow(T x)
    {
        // cap elements to prevent overflow
        if (_capacity >= ((std::numeric_limits<uint32_t>::max)() >> 1))
        {
            throw std::bad_alloc();
        }

        T* new_data = new T[_capacity <<= 1];
        memcpy(new_data, _data, _size * sizeof(T));
        memfree();
        (_data = new_data)[_size++] = x;
    }

    void memfree()
    {
        if (_data != _insitu)
            delete [] _data;
    }

    BOOST_STATIC_ASSERT(is_pod<T>::value);

    uint32_t _size;
    uint32_t _capacity;
    T        _insitu[N];
    T*       _data;
};

} // namespace detail
} // namespace bond
