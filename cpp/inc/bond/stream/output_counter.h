// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/blob.h>

namespace bond
{

template <typename T, uint32_t N>
struct VariableUnsigned
{
    static void Write(uint32_t& count, T value)
    {
        BOOST_STATIC_ASSERT(N < 10);

        if (value)
            VariableUnsigned<T, N+1>::Write(count, value >> 7);
        else
            count += N;
    }
};

template <>
struct VariableUnsigned<uint64_t, 10>
{
    static void Write(uint32_t& count, uint64_t /*value*/)
    {
        count += 10;
    }
};

template <>
struct VariableUnsigned<uint32_t, 5>
{
    static void Write(uint32_t& count, uint32_t /*value*/)
    {
        count += 5;
    }
};

template <>
struct VariableUnsigned<uint16_t, 3>
{
    static void Write(uint32_t& count, uint16_t /*value*/)
    {
        count += 3;
    }
};

class OutputCounter
{

    struct Buffer
    {
        uint32_t _size;

        uint32_t size() const
        {
            return _size;
        }
    };

public:
    OutputCounter()
        : _count(0)
    {}

    uint32_t GetCount() const
    {
        return _count;
    }

    template<typename T>
    void Write(const T&)
    {
        _count += sizeof(T);
    }

    void Write(const void*, uint32_t size)
    {
        _count += size;
    }

    void Write(const blob& buffer)
    {
        _count += buffer.size();
    }

    void Write(const Buffer& buffer)
    {
        _count += buffer.size();
    }

    template<typename T>
    void WriteVariableUnsigned(T value)
    {
        VariableUnsigned<T, 1>::Write(_count, value >> 7);
    }

    Buffer GetBuffer() const
    {
        return { GetCount() };
    }

private:
    uint32_t _count;
};


inline OutputCounter CreateOutputBuffer(const OutputCounter& /*other*/)
{
    return OutputCounter();
}


} // namespace bond
