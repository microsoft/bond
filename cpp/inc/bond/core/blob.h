// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

/** @file */
#pragma once

#include <bond/core/config.h>

#include "container_interface.h"
#include "detail/checked.h"

#include <boost/make_shared.hpp>
#include <boost/shared_array.hpp>

#include <stdint.h>
#include <cstring>

/// namespace bond
namespace bond
{

/// @brief Memory blob
class blob
{
public:
    typedef int8_t value_type;
    typedef const char* const_iterator;

    /// @brief Default constructor
    blob()
        : _buffer(),
          _content(),
          _length()
    {
    }

    /// @brief Construct from a raw pointer to memory buffer
    ///
    /// Not recommended because of buffer lifetime management.
    blob(const void* content, uint32_t length)
        : _buffer(),
          _content(static_cast<const char*>(content)),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Construct from a boost::shared_ptr to const memory buffer
    blob(const boost::shared_ptr<const char[]>& buffer, uint32_t length)
        : _buffer(buffer),
          _content(_buffer.get()),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Construct from a boost::shared_ptr to const memory buffer
    blob(const boost::shared_ptr<const char[]>& buffer, uint32_t offset, uint32_t length)
        : _buffer(buffer),
          _content(bond::detail::checked_add(_buffer.get(), offset)),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Construct from a boost::shared_ptr to memory buffer
    blob(const boost::shared_ptr<char[]>& buffer, uint32_t length)
        : _buffer(buffer),
          _content(_buffer.get()),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Construct from a boost::shared_ptr to memory buffer
    blob(const boost::shared_ptr<char[]>& buffer, uint32_t offset, uint32_t length)
        : _buffer(buffer),
          _content(bond::detail::checked_add(_buffer.get(), offset)),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Construct from a smart pointer other than boost::shared_ptr
    ///
    /// Not recommended for performance reasons. Use boost::shared_ptr whenever possible.
    template <typename T, template <typename U> class SmartPtr>
    blob(const SmartPtr<T>& buffer, uint32_t length)
        : _buffer(wrap_in_shared_ptr(buffer)),
          _content(_buffer.get()),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Construct from a smart pointer other than boost::shared_ptr
    ///
    /// Not recommended for performance reasons. Use boost::shared_ptr whenever possible.
    template <typename T, template <typename U> class SmartPtr>
    blob(const SmartPtr<T>& buffer, uint32_t offset, uint32_t length)
        : _buffer(wrap_in_shared_ptr(buffer)),
          _content(bond::detail::checked_add(_buffer.get(), offset)),
          _length(length)
    {
        bond::detail::checked_add(_content, length);
    }

    /// @brief Move constructor
    blob(blob&& that) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<boost::shared_ptr<const char[]> >::value)
        : _buffer(std::move(that._buffer)),
          _content(std::move(that._content)),
          _length(std::move(that._length))
    {
        that._content = 0;
        that._length = 0;
    }

    blob(const blob& that) = default;
    blob& operator=(const blob& that) = default;

    /// @brief Assign a new value from another blob object or its part
    void assign(const blob& from, uint32_t offset, uint32_t length)
    {
        if (bond::detail::checked_add(offset, length) > from._length)
        {
            throw std::invalid_argument("Total of offset and length too large; must be less than or equal to length of blob");
        }

        _buffer = from._buffer;
        _content = from._content + offset;
        _length = length;
    }

    /// @brief Assign a new value from a raw or smart pointer
    template <typename T>
    void assign(const T& buffer, uint32_t length)
    {
        blob temp(buffer, length);

        swap(temp);
    }

    /// @brief Assign a new value from a raw or smart pointer
    template <typename T>
    void assign(const T& buffer, uint32_t offset, uint32_t length)
    {
        blob temp(buffer, offset, length);

        swap(temp);
    }

    /// @brief Return a blob object for a range of this object
    blob range(uint32_t offset, uint32_t length) const
    {
        if (bond::detail::checked_add(offset, length) > _length)
        {
            throw std::invalid_argument("Total of offset and length too large; must be less than or equal to length of blob");
        }

        blob temp;
        temp._buffer = _buffer;
        temp._content = _content + offset;
        temp._length = length;

        return temp;
    }

    /// @brief Return a blob object for a range from the specified offset to
    /// the end of the buffer
    blob range(uint32_t offset) const
    {
        if (offset > _length)
        {
            throw std::invalid_argument("Offset too large; must be less than or equal to length of blob");
        }

        blob temp = *this;
        temp._content += offset;
        temp._length -= offset;

        return temp;
    }

    /// @brief Swap with another blob
    void swap(blob& src)
    {
        std::swap(_content, src._content);
        std::swap(_length, src._length);
        _buffer.swap(src._buffer);
    }

    /// @brief Clear reference to the underlying memory buffer and reset the
    /// blob to empty
    void clear()
    {
        blob temp;

        swap(temp);
    }

    /// @brief Pointer to the content
    const char* content() const
    {
        return _content;
    }

    /// @brief Void pointer to the content
    const void* data() const
    {
        return _content;
    }

    /// @brief Length of the content
    uint32_t length() const
    {
        return _length;
    }

    /// @brief Length of the content
    uint32_t size() const
    {
        return _length;
    }

    /// @brief Check if the blob is empty (i.e. lenght == 0)
    bool empty() const
    {
        return 0 == length();
    }

    bool operator==(const blob& src) const
    {
        return this == &src
               || ((_length == src._length)
                   && (0 == ::memcmp(_content, src._content, _length)));
    }

    /// @brief Iterator for the beginning of the blob
    const_iterator begin() const
    {
        return _content;
    }

    /// @brief Iterator for the end of the blob
    const_iterator end() const
    {
        return _content + _length;
    }


    template <typename T>
    friend T blob_cast(const blob& from);

private:
    template <typename T>
    struct deleter
    {
        deleter(const T& p)
            : p(p)
        {}

        void operator()(void const *)
        {
            p.reset();
        }

        T p;
    };

    template <typename T, template <typename U> class SmartPtr>
    static boost::shared_ptr<const char[]> wrap_in_shared_ptr(const SmartPtr<T>& p)
    {
        boost::shared_ptr<const char[]> ptr(static_cast<const char*>(static_cast<const void*>(p.get())),
                                            deleter<SmartPtr<T> >(p));
        return ptr;
    }

    boost::shared_ptr<const char[]> _buffer;

    const char* _content;

    uint32_t _length;
};

/// @brief Swap two blobs
inline void swap(blob& src, blob& dst)
{
    src.swap(dst);
}

inline bool operator != (const blob& x, const blob& y)
{
    return !(x == y);
}

template <typename A>
inline blob merge(const A& allocator, const blob& x, const blob& y)
{
    if (x.empty() || y.empty())
    {
        //
        // one of provided blobs is empty,
        // return the other one
        //
        return x.empty() ? y : x;
    }
    else
    {
        uint32_t length = detail::checked_add(x.length(), y.length());
        boost::shared_ptr<char[]> buffer = boost::allocate_shared_noinit<char[]>(allocator, length);

        ::memcpy(buffer.get(), x.content(), x.length());
        ::memcpy(buffer.get() + x.length(), y.content(), y.length());

        return blob(buffer, length);
    }
}

template <typename t_It, typename A>
inline blob merge(const A& allocator, t_It begin, t_It end)
{
    //
    // calculate the size of resulting blob
    //
    uint32_t length = 0;
    for (t_It it = begin; it != end; ++it)
    {
        length = detail::checked_add(length, it->length());
    }

    if (0 == length)
    {
        //
        // empty blob to return
        //
        return blob();
    }
    else if (length == begin->length())
    {
        //
        // just first blob in the sequence is not empty
        //
        return *begin;
    }
    else
    {
        //
        // sequence of several non-empty blobs
        //
        BOOST_ASSERT(length > begin->length());

        boost::shared_ptr<char[]> buffer = boost::allocate_shared_noinit<char[]>(allocator, length);

        uint32_t offset = 0;
        for (t_It it = begin; it != end; ++it)
        {
            ::memcpy(buffer.get() + offset, it->content(), it->length());

            offset += it->length();
        }

        BOOST_ASSERT(offset == length);
        return blob(buffer, length);
    }
}

inline blob merge(const blob& x, const blob& y)
{
    return merge(std::allocator<char>(), x, y);
}

template <typename t_It>
inline blob merge(t_It begin, t_It end)
{
    return merge(std::allocator<char>(), begin, end);
}

template <> struct
is_list_container<blob>
    : std::true_type {};


template <typename T>
inline T blob_cast(const blob& from)
{
    if (from._buffer)
    {
        boost::shared_array<char> ptr(const_cast<char*>(static_cast<const char*>(static_cast<const void*>(from._buffer.get()))),
                                      blob::deleter<boost::shared_ptr<const char[]> >(from._buffer));

        return T(ptr, static_cast<uint32_t>(from._content - from._buffer.get()), from._length);
    }
    else
    {
        return T(from._content, from._length);
    }
}


} // namespace bond
