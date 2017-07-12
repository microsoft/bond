// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <string>
#include <vector>
#include <stdint.h>
#include <stdio.h>

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

    basic_string_stream& operator<<(int value)
    {
        char str[256];
        *str = '\0';

        ::sprintf(str, "%d", value);
        return *this << str;
    }

    basic_string_stream& operator<<(unsigned int value)
    {
        char str[256];
        *str = '\0';

        ::sprintf(str, "%u", value);
        return *this << str;
    }

    basic_string_stream& operator<<(long value)
    {
        char str[256];
        *str = '\0';

        ::sprintf(str, "%ld", value);
        return *this << str;
    }

    basic_string_stream& operator<<(unsigned long value)
    {
        char str[256];
        *str = '\0';

        ::sprintf(str, "%lu", value);
        return *this << str;
    }

    basic_string_stream& operator<<(long long value)
    {
        char str[256];
        *str = '\0';

        ::sprintf(str, "%lld", value);
        return *this << str;
    }

    basic_string_stream& operator<<(unsigned long long value)
    {
        char str[256];
        *str = '\0';
        
        ::sprintf(str, "%llu", value);
        return *this << str;
    }

    basic_string_stream& operator<<(double value)
    {
        char str[256];
        *str = '\0';

        ::sprintf(str, "%f", value);
        return *this << str;
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
