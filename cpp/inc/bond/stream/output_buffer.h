// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/** @file */
#pragma once

#include <bond/core/config.h>

#include <bond/core/blob.h>
#include <bond/core/containers.h>
#include <bond/core/detail/checked.h>
#include <bond/core/traits.h>
#include <boost/static_assert.hpp>
#include <cstring>
#include <limits>
#include <stdexcept>

namespace bond
{

namespace output_buffer
{

template <typename T, uint32_t N>
struct VariableUnsignedUnchecked
{
    BOOST_STATIC_ASSERT(N < 10);

    static uint32_t Write(char* p, T value)
    {
        uint8_t byte = static_cast<uint8_t>(value);

        if (value >>= 7)
        {
            byte |= 0x80;
            std::memcpy(p + N - 1, &byte, 1);
            return VariableUnsignedUnchecked<T, N+1>::Write(p, value);
        }
        else
        {
            std::memcpy(p + N - 1, &byte, 1);
            return N;
        }
    }
};


template <>
struct VariableUnsignedUnchecked<uint64_t, 10>
{
    static uint32_t Write(char* p, uint64_t value)
    {
        BOOST_VERIFY(value == 1);
        const uint8_t byte = 1;
        std::memcpy(p + 9, &byte, 1);
        return 10;
    }
};


template <>
struct VariableUnsignedUnchecked<uint32_t, 5>
{
    static uint32_t Write(char* p, uint32_t value)
    {
        const uint8_t byte = static_cast<uint8_t>(value);
        std::memcpy(p + 4, &byte, 1);
        return 5;
    }
};


template <>
struct VariableUnsignedUnchecked<uint16_t, 3>
{
    static uint32_t Write(char* p, uint16_t value)
    {
        const uint8_t byte = static_cast<uint8_t>(value);
        std::memcpy(p + 2, &byte, 1);
        return 3;
    }
};

}

/// @brief Memory backed output stream
template <typename A = std::allocator<char> >
class OutputMemoryStream
{
public:
    /// @brief Construct OutputMemoryStream using specified allocator instance
    explicit OutputMemoryStream(const A& allocator = A())
        : _allocator(allocator),
          _buffer(),
          _bufferSize(0),
          _rangeSize(0),
          _rangeOffset(0),
          _minChainningSize(32),
          _maxChainLength((uint32_t)-1),
          _rangePtr(0),
          _blobs(_allocator)
    {}

    /// @brief Construct OutputMemoryStream from the first buffer of the specified
    /// size and a preallocated vector to store additional buffers.
    explicit OutputMemoryStream(const boost::shared_ptr<char[]>& buffer,
                                uint32_t size,
                                uint32_t reserveBlobs = 128,
                                const A& allocator = A(),
                                uint32_t minChanningSize = 32,
                                uint32_t maxChainLength = (uint32_t)-1)
        : _allocator(allocator),
          _buffer(buffer),
          _bufferSize(size),
          _rangeSize(0),
          _rangeOffset(0),
          _minChainningSize(minChanningSize),
          _maxChainLength(maxChainLength),
          _rangePtr(_buffer.get()),
          _blobs(allocator)
    {
        _blobs.reserve(reserveBlobs);
    }


    /// @brief Construct OutputMemoryStream with the first buffer of the specified
    /// size and a preallocated vector to store additional buffers.
    explicit OutputMemoryStream(uint32_t reserveSize,
                                uint32_t reserveBlobs = 128,
                                const A& allocator = A(),
                                uint32_t minChanningSize = 32,
                                uint32_t maxChainLength = (uint32_t)-1)
        : _allocator(allocator),
          _buffer(boost::allocate_shared_noinit<char[]>(_allocator, reserveSize)),
          _bufferSize(reserveSize),
          _rangeSize(0),
          _rangeOffset(0),
          _minChainningSize(minChanningSize),
          _maxChainLength(maxChainLength),
          _rangePtr(_buffer.get()),
          _blobs(allocator)
    {
        _blobs.reserve(reserveBlobs);
    }


    /// @brief Get content of the stream as a collection of memory blobs
    /// @remarks The provided collection must have reserve, assign and emplace_back functions
    template <typename T>
    void GetBuffers(T& buffers) const
    {
        buffers.reserve(bond::detail::checked_add(_blobs.size(), 1U));

        //
        // insert all "ready" blobs
        //
        buffers.assign(_blobs.begin(), _blobs.end());

        if (_rangeSize > 0)
        {
            //
            // attach current array, if not empty,
            // as a last blob
            //
            buffers.emplace_back(_buffer, _rangeOffset, _rangeSize);
        }
    }

    /// @brief Get content of the stream as one contiguous memory blob
    ///
    /// The function may need to allocate memory and perform memcpy.
    blob GetBuffer() const
    {
        //
        // merge all blobs in the active list with the current one
        //
        blob current(_buffer, _rangeOffset, _rangeSize);
        return merge(_allocator, merge(_allocator, _blobs.begin(), _blobs.end()), current);
    }


    template<typename T>
    void Write(const T& value)
    {
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386) || defined(_M_IX86)
        //
        // x86/x64 performance tweak: for small types don't go into generic
        // Write(), which results in call to memcpy() and additional memory
        // access. The direct copy of the value (which is likely on register
        // already) is faster.
        //
        if (sizeof(T) + _rangeSize + _rangeOffset <= _bufferSize)
        {
            *reinterpret_cast<T*>(_rangePtr + _rangeSize) = value;
            _rangeSize += sizeof(T);
        }
        else
        {
            Write(&value, sizeof(value));
        }
#else
        //
        // We can't use the trick above on platforms that don't support
        // unaligned memory access, so fall back to the version of Write
        // that is implemented with memcpy. memcpy handles the alignment for
        // us.
        //
        Write(&value, sizeof(value));
#endif
    }


    void Write(const void* value, uint32_t size)
    {
        uint32_t    sizePart = _bufferSize - _rangeSize - _rangeOffset;
        const char* buffer = static_cast<const char*>(value);

        if (sizePart > size)
        {
            sizePart = size;
        }

        //
        // Copy to the tail of current range. Note that _rangePtr may still be
        // null here on initial write to an empty buffer. The behaviour of
        // std::memcpy() is undefined in that situation, even if size == 0.    
        //
        if (sizePart > 0)
        {
            std::memcpy(_rangePtr + _rangeSize,
                        buffer,
                        sizePart);

            // increase current range size
            _rangeSize += sizePart;
        }

        //
        // if there is more bytes to copy, allocate a new buffer
        //
        if (size != sizePart)
        {
            BOOST_ASSERT(_bufferSize == _rangeSize + _rangeOffset);

            //
            // shift input data window by size of copied bytes, if any
            //
            size -= sizePart;
            buffer += sizePart;

            //
            // snap current range to internal list of blobs, if not empty
            //
            if (_rangeSize > 0)
            {
                _blobs.emplace_back(_buffer, _rangeOffset, _rangeSize);
            }

            // cap buffer to prevent overflow
            if (_bufferSize > ((std::numeric_limits<uint32_t>::max)() >> 1))
            {
                throw std::bad_alloc();
            }

            //
            // grow buffer by 50% (at least 4096 bytes for initial buffer)
            // and enough to store left overs of specified buffer
            //
            _bufferSize += _bufferSize ? _bufferSize / 2 : 4096;
            _bufferSize = (std::max)(_bufferSize, size);

            _buffer = boost::allocate_shared_noinit<char[]>(_allocator, _bufferSize);

            //
            // init range
            //
            _rangeOffset = 0;
            _rangePtr = _buffer.get();
            _rangeSize = size;

            //
            // copy to the tail of current range
            std::memcpy(_rangePtr,
                        buffer,
                        size);
        }
    }

    void Write(const blob& buffer)
    {
        if (buffer.size() < _minChainningSize || _blobs.size() >= _maxChainLength)
        {
            // For small amount of data it's faster to memcpy it than chain blob
            return Write(buffer.data(), buffer.size());
        }

        //
        // Internal list of blobs must represent valid sequence of bytes

        //
        // First snap current buffer range, if it is not empty
        //
        if (_rangeSize > 0)
        {
            _blobs.emplace_back(_buffer, _rangeOffset, _rangeSize);

            _rangeOffset += _rangeSize;
            _rangePtr += _rangeSize;

            //
            // reset range size
            //
            _rangeSize = 0;
        }

        //
        // attach specified blob to the end of the list
        //
        _blobs.push_back(buffer);
    }

    void Flush()
    {
        //
        // nop
        //
    }

    template<typename T>
    void WriteVariableUnsigned(T value)
    {
        if (sizeof(T) * 8 / 7 + _rangeSize + _rangeOffset < _bufferSize)
        {
            char* ptr = _rangePtr + _rangeSize;
            _rangeSize += output_buffer::VariableUnsignedUnchecked<T, 1>::Write(ptr, value);
        }
        else
        {
            GenericWriteVariableUnsigned(*this, value);
        }
    }

protected:
    // allocator instance
    A _allocator;

    // current buffer
    boost::shared_ptr<char[]> _buffer;

    // size of current buffer
    uint32_t _bufferSize;

    // size of current buffer range
    uint32_t _rangeSize;

    // offset of current buffer range
    uint32_t _rangeOffset;

    // smallest blob size that will be chained rather than copied
    uint32_t _minChainningSize;

    // maximum length of the chains of buffers
    uint32_t _maxChainLength;

    // pointer of current buffer range
    char* _rangePtr;

    // list of blobs
    std::vector<blob, typename std::allocator_traits<A>::template rebind_alloc<blob> > _blobs;


    friend OutputMemoryStream CreateOutputBuffer(const OutputMemoryStream& other)
    {
        return OutputMemoryStream(
            other._bufferSize,
            static_cast<uint32_t>(other._blobs.capacity()),
            other._allocator,
            other._minChainningSize,
            other._maxChainLength);
    }

}; // class OutputMemoryStream


/// @brief Type alias for memory backed output stream using std::allocator
typedef OutputMemoryStream<> OutputBuffer;

} // namespace bond
