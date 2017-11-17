// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

namespace bond
{

namespace detail
{

template<uint16_t Size, typename Allocator = std::allocator<char> >
class basic_string_stream
{
public:
    basic_string_stream()
    {
        buffer.reserve(Size);
        buffer.push_back('\0');
    }

    explicit basic_string_stream(const Allocator& allocator)
        : buffer(allocator)
    {
        buffer.reserve(Size);
        buffer.push_back('\0');
    }

    basic_string_stream& operator<<(const char* str)
    {
        while (*str)
        {
            write(*str++);
        }

        return *this;
    }

    template<typename T, typename A>
    basic_string_stream& operator<<(const std::basic_string<char, T, A>& str)
    {
        write(str.begin(), str.end());
        return *this;
    }

    basic_string_stream& operator<<(char value)
    {
        write(value);
        return *this;
    }

    template <typename T>
    basic_string_stream& operator<<(const T& value)
    {
        return *this << std::to_string(value);
    }

    std::string str() const
    {
        return content();
    }

    const char* content() const
    {
        return &buffer[0];
    }

private:
    void write(char ch)
    {
        buffer.back() = ch;
        buffer.push_back('\0');
    }

    template<typename I>
    void write(I begin, I end)
    {
        for ( ; begin != end; ++begin)
        {
            write(*begin);
        }
    }

    std::vector<char, Allocator> buffer;
};

typedef basic_string_stream<1024> string_stream;

}

}
