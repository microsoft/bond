// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/blob.h>
#include <bond/core/containers.h>
#include <bond/core/traits.h>

#include <boost/static_assert.hpp>

#include <cstring>

namespace bond
{

template <typename T>
class RandomProtocolEngine
{
public:
    // We don't need a good pseudo-random number generator but we do need one
    // that is consistent accross compilers/libraries so that we can use
    // randomly generated data files from one platfom to verify compatibility
    // on another platform.
    RandomProtocolEngine()
    {}

    // Variant of Marsaglia's xorshift generator
    // http://arxiv.org/pdf/1404.0390v1.pdf
    uint64_t Next()
    {
        uint64_t s1 = state[0];
        const uint64_t s0 = state[1];
        state[0] = s0;
        s1 ^= s1 << 23;
        return (state[1] = (s1 ^ s0 ^ (s1 >> 17) ^ (s0 >> 26))) + s0;
    }

    void Seed(uint64_t s0 = seed1, uint64_t s1 = seed2)
    {
        state[0] = s0;
        state[1] = s1;
    }

    static uint64_t state[2];
    const static uint64_t seed1 = 234578354U;
    const static uint64_t seed2 = 753478U;
};

template <typename T>
uint64_t RandomProtocolEngine<T>::state[2] = {seed1, seed2};

//
// Protocol which generates a random stream of bits.
// In some cases it may be useful for initializing data in tests, e.g.:
//
// Params param;
// Apply(bond::To<Params>(param), bond::bonded<Params>(bond::RandomProtocolReader()));
//
class RandomProtocolReader
    : public RandomProtocolEngine<RandomProtocolReader>
{
public:
    typedef bond::StaticParser<RandomProtocolReader&> Parser;

    RandomProtocolReader(uint32_t max_string_length = 50, uint32_t max_list_size = 20, bool json = false)
        : _max_string_length(max_string_length),
          _max_list_size(max_list_size),
          _json(json)
    {}

    bool operator==(const RandomProtocolReader&) const
    {
        return false;
    }

    bool ReadVersion()
    {
        return false;
    }

    template <typename T>
    typename boost::disable_if<is_string_type<T> >::type
    Read(T& value)
    {
        BOOST_STATIC_ASSERT(std::is_trivially_copyable<T>::value);
        // We only have 64 bits of randomness, so T needs to fit in that.
        BOOST_STATIC_ASSERT(sizeof(T) <= sizeof(uint64_t));

        uint64_t random = RandomProtocolEngine::Next();
        std::memcpy(&value, &random, sizeof(T));
    }

    void Read(uint64_t& value)
    {
        value = RandomProtocolEngine::Next();

        if (_json)
        {
            // NewtonsoftJson used by C# implementation doesn't support
            // numbers larger that max int64.
            value >>= 1;
        }
    }

    void Read(bool& value)
    {
        int8_t n;
        Read(n);
        value = n > 0;
    }

    void Read(double& value)
    {
        BOOST_STATIC_ASSERT(sizeof(double) == sizeof(uint64_t));

        uint8_t sign;
        uint8_t exponent;
        uint32_t mantissa;

        Read(sign);
        Read(exponent);
        Read(mantissa);

        // don't return special values: infinity, NaN
        if (exponent == 0)
            exponent = 0x80;

        uint64_t bits = ((uint64_t)(sign) << 63) | ((uint64_t)(exponent) << (52 + 3)) | (uint64_t)mantissa;
        std::memcpy(&value, &bits, sizeof(bits));
    }

    void Read(float& value)
    {
        BOOST_STATIC_ASSERT(sizeof(float) == sizeof(uint32_t));

        uint8_t sign;
        uint8_t exponent;
        uint16_t mantissa;

        Read(sign);
        Read(exponent);
        Read(mantissa);

        // don't return special values: infinity, NaN
        if (exponent == 0 || exponent == 0xff)
            exponent = 0x80;

        uint32_t bits = ((uint32_t)(sign) << 31) | ((uint32_t)(exponent) << 23) | (uint32_t)mantissa;
        std::memcpy(&value, &bits, sizeof(bits));
    }

    template <typename T>
    typename boost::enable_if<is_string_type<T> >::type
    Read(T& value)
    {
        uint32_t length = 0;

        Read(length);

        length %= _max_string_length;
        length = length ? length : 1;

        resize_string(value, length);

        typename element_type<T>::type* p = string_data(value);
        typename element_type<T>::type* const p_end = p + length;

        for (; p != p_end; ++p)
        {
            uint8_t c;

            Read(c);

            *p = typename element_type<T>::type(' ') + c % ('z' - ' ');
        }
    }

    // Read for blob
    void Read(blob& value, uint32_t size)
    {
        boost::shared_ptr<char[]> buffer(boost::make_shared_noinit<char[]>(size));
        char* p = buffer.get();
        char* const p_end = p + size;

        for (; p != p_end; ++p)
        {
            Read(*p);
        }

        value.assign(buffer, size);
    }

    template <typename T>
    void Skip()
    {}

    template <typename T>
    void Skip(const T&)
    {}

    template <typename T>
    void ReadContainerBegin(uint32_t& size, T&)
    {
        Read(size);

        size %= _max_list_size;
        size = size ? size : 1;
    }

    void ReadContainerEnd() const
    {
    }

private:
    uint32_t _max_string_length;
    uint32_t _max_list_size;
    bool _json;
};


template <typename Unused> struct
uses_marshaled_bonded<RandomProtocolReader, Unused>
    : std::false_type {};

template <typename Unused> struct
uses_marshaled_bonded<RandomProtocolReader&, Unused>
    : std::false_type {};

}
