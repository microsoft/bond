// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/blob.h>
#include <bond/core/exception.h>
#include <bond/core/traits.h>
#include <boost/static_assert.hpp>
#include <cstring>

namespace bond
{

namespace input_buffer
{

template <typename T, uint32_t Shift>
struct VariableUnsignedUnchecked
{
    BOOST_STATIC_ASSERT(Shift < 56);
    static void Read(const char*& p, T& value)
    {
        T byte = *p++;
        value |= (byte & 0x7f) << Shift;

        if (byte >= 0x80)
            VariableUnsignedUnchecked<T, Shift + 7>::Read(p, value);
    }
};


// Specialization for the first byte
template <typename T>
struct VariableUnsignedUnchecked<T, 0>
{
    static void Read(const char*& p, T& value)
    {
        T byte = *p++;
        value = (byte & 0x7f);

        if (byte >= 0x80)
            VariableUnsignedUnchecked<T, 7>::Read(p, value);
    }
};


// Specialization for the last byte of uint16_t
template <>
struct VariableUnsignedUnchecked<uint16_t, 14>
{
    static void Read(const char*& p, uint16_t& value)
    {
        uint16_t byte = *p++;
        value |= byte << 14;
    }
};


// Specialization for the last byte of uint32_t
template <>
struct VariableUnsignedUnchecked<uint32_t, 28>
{
    static void Read(const char*& p, uint32_t& value)
    {
        uint32_t byte = *p++;
        value |= byte << 28;
    }
};


// Specialization for the last 2 bytes of uint64_t
template <>
struct VariableUnsignedUnchecked<uint64_t, 56>
{
    static void Read(const char*& p, uint64_t& value)
    {
        uint64_t byte = *p++;
        value |= byte << 56;

        if (byte >= 0x80)
            p++;
    }
};

}

/// @brief Memory backed input stream
class InputBuffer
{
public:
    /// @brief Default constructor
    InputBuffer()
        : _pointer()
    {}

    /// @brief Construct from a blob
    ///
    /// InputBuffer makes a copy of the blob. Assuming that the blob was
    /// created using a ref-counted smart pointer this assures proper lifetime
    /// management for the underlying memory buffer.
    InputBuffer(const blob& blob)
        : _blob(blob),
          _pointer()
    {}

    /// @brief Construct form a raw memory pointer
    ///
    /// Pointer(s) to the memory buffer may be held by the objects deserialized
    /// from the stream. It is the application's responsibility to manage
    /// the lifetime of the memory buffer appropriately.
    InputBuffer(const void* buffer, uint32_t length)
        : _blob(buffer, length),
          _pointer()
    {}


    bool operator==(const InputBuffer& rhs) const
    {
        return _blob == rhs._blob
            && _pointer == rhs._pointer;
    }


    void Read(uint8_t& value)
    {
        if (_blob.length() == _pointer)
        {
            EofException(sizeof(uint8_t));
        }

        value = static_cast<const uint8_t>(_blob.content()[_pointer++]);
    }


    template <typename T>
    void Read(T& value)
    {
        BOOST_STATIC_ASSERT(std::is_arithmetic<T>::value || std::is_enum<T>::value);

        if (sizeof(T) > _blob.length() - _pointer)
        {
            EofException(sizeof(T));
        }

#if defined(__x86_64__) || defined(_M_X64) || defined(__i386) || defined(_M_IX86)
        // x86/x64 performance tweak: we can access memory unaligned, so
        // read directly from the buffer. Benchmarks show this as being
        // slightly faster than memcpy.
        value = *reinterpret_cast<const T*>(_blob.content() + _pointer);
#else
        //
        // We can't use the trick above on platforms that don't support
        // unaligned memory access, so just use memcpy.
        //
        const void* const src = _blob.content() + _pointer;
        std::memcpy(&value, src, sizeof(T));
#endif

        _pointer += sizeof(T);
    }


    void Read(void *buffer, uint32_t size)
    {
        if (size > _blob.length() - _pointer)
        {
            EofException(size);
        }

        const void* const src = _blob.content() + _pointer;
        std::memcpy(buffer, src, size);

        _pointer += size;
    }


    void Read(blob& blob, uint32_t size)
    {
        if (size > _blob.length() - _pointer)
        {
            EofException(size);
        }

        blob.assign(_blob, _pointer, size);

        _pointer += size;
    }


    void Skip(uint32_t size)
    {
        if (size > _blob.length() - _pointer)
        {
            return;
        }

        _pointer += size;
    }

    /// @brief Check if the stream is at the end of the underlying memory buffer.
    bool IsEof() const
    {
        return _pointer == _blob.length();
    }


    template <typename T>
    void ReadVariableUnsigned(T& value)
    {
        if (_blob.length() > _pointer + sizeof(T) * 8 / 7)
        {
            const char* ptr = _blob.content() + _pointer;
            input_buffer::VariableUnsignedUnchecked<T, 0>::Read(ptr, value);
            _pointer = static_cast<uint32_t>(ptr - _blob.content());
        }
        else
        {
            GenericReadVariableUnsigned(*this, value);
        }
    }

protected:
    [[noreturn]] void EofException(uint32_t size) const
    {
        BOND_THROW(StreamException,
              "Read out of bounds: " << size << " bytes requested, offset: "
              << _pointer << ", length: " << _blob.length());
    }

    blob        _blob;
    uint32_t    _pointer;


    friend blob GetCurrentBuffer(const InputBuffer& input)
    {
        return input._blob.range(input._pointer);
    }
};


inline InputBuffer CreateInputBuffer(const InputBuffer& /*other*/, const blob& blob)
{
    return InputBuffer(blob);
}

inline blob GetBufferRange(const blob& begin, const blob& end)
{
    return begin.range(0, begin.length() - end.length());
}

BOND_DEFINE_BUFFER_MAGIC(InputBuffer, 0x4249 /*IB*/);

} // namespace bond
