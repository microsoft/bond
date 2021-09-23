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
    blob() BOND_NOEXCEPT
        : _buffer(),
          _length()
    {
    }

    /// @brief Construct from a raw pointer to memory buffer
    ///
    /// Not recommended because of buffer lifetime management.
    blob(const void* content, uint32_t length)
        : _buffer(boost::shared_ptr<const void>(), static_cast<const char*>(content)),  // Alias to an empty shared_ptr in order to store
                                                                                        // only a content pointer without the control block.
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Construct from a boost::shared_ptr to const memory buffer
    blob(const boost::shared_ptr<const char[]>& buffer, uint32_t length)
        : _buffer(buffer),
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Construct from a boost::shared_ptr to const memory buffer
    blob(const boost::shared_ptr<const char[]>& buffer, uint32_t offset, uint32_t length)
        : _buffer(buffer, bond::detail::checked_add(buffer.get(), offset)),
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Construct from a boost::shared_ptr to memory buffer
    blob(const boost::shared_ptr<char[]>& buffer, uint32_t length)
        : _buffer(buffer),
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Construct from a boost::shared_ptr to memory buffer
    blob(const boost::shared_ptr<char[]>& buffer, uint32_t offset, uint32_t length)
        : _buffer(buffer, bond::detail::checked_add(buffer.get(), offset)),
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Construct from a smart pointer other than boost::shared_ptr
    ///
    /// Not recommended for performance reasons. Use boost::shared_ptr whenever possible.
    template <typename T, template <typename U> class SmartPtr>
    blob(const SmartPtr<T>& buffer, uint32_t length)
        : _buffer(wrap_in_shared_ptr(buffer)),
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Construct from a smart pointer other than boost::shared_ptr
    ///
    /// Not recommended for performance reasons. Use boost::shared_ptr whenever possible.
    template <typename T, template <typename U> class SmartPtr>
    blob(const SmartPtr<T>& buffer, uint32_t offset, uint32_t length)
        : _buffer(wrap_in_shared_ptr(buffer, offset)),
          _length(length)
    {
        bond::detail::checked_add(_buffer.get(), length);
    }

    /// @brief Move constructor
    blob(blob&& that) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_constructible<boost::shared_ptr<const char[]> >::value)
        : _buffer(std::move(that._buffer)),
          _length(std::move(that._length))
    {
        that._length = 0;
    }

    blob& operator=(blob&& that) BOND_NOEXCEPT_IF(
        std::is_nothrow_move_assignable<boost::shared_ptr<const char[]> >::value)
    {
        _buffer = std::move(that._buffer);
        _length = std::move(that._length);
        that._length = 0;
        return *this;
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

        _buffer = boost::shared_ptr<const char[]>(from._buffer, from._buffer.get() + offset);
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

        return blob(_buffer, offset, length);
    }

    /// @brief Return a blob object for a range from the specified offset to
    /// the end of the buffer
    blob range(uint32_t offset) const
    {
        if (offset > _length)
        {
            throw std::invalid_argument("Offset too large; must be less than or equal to length of blob");
        }

        return blob(_buffer, offset, _length - offset);
    }

    /// @brief Swap with another blob
    void swap(blob& src) BOND_NOEXCEPT
    {
        std::swap(_length, src._length);
        _buffer.swap(src._buffer);
    }

    /// @brief Clear reference to the underlying memory buffer and reset the
    /// blob to empty
    void clear() BOND_NOEXCEPT
    {
        blob temp;

        swap(temp);
    }

    /// @brief Pointer to the content
    const char* content() const BOND_NOEXCEPT
    {
        return _buffer.get();
    }

    /// @brief Void pointer to the content
    const void* data() const BOND_NOEXCEPT
    {
        return _buffer.get();
    }

    /// @brief Length of the content
    uint32_t length() const BOND_NOEXCEPT
    {
        return _length;
    }

    /// @brief Length of the content
    uint32_t size() const BOND_NOEXCEPT
    {
        return _length;
    }

    /// @brief Check if the blob is empty (i.e. lenght == 0)
    bool empty() const BOND_NOEXCEPT
    {
        return 0 == length();
    }

    bool operator==(const blob& src) const BOND_NOEXCEPT
    {
        return this == &src
               || ((_length == src._length)
                   && (0 == ::memcmp(_buffer.get(), src._buffer.get(), _length)));
    }

    /// @brief Iterator for the beginning of the blob
    const_iterator begin() const BOND_NOEXCEPT
    {
        return _buffer.get();
    }

    /// @brief Iterator for the end of the blob
    const_iterator end() const BOND_NOEXCEPT
    {
        return _buffer.get() + _length;
    }


    template <typename T>
    friend T blob_cast(const blob& from);

    template <typename A>
    friend blob blob_prolong(blob src, const A& allocator);

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
    static boost::shared_ptr<const char[]> wrap_in_shared_ptr(const SmartPtr<T>& p, uint32_t offset = 0)
    {
        boost::shared_ptr<const char[]> ptr(bond::detail::checked_add(static_cast<const char*>(static_cast<const void*>(p.get())), offset),
                                            deleter<SmartPtr<T> >(p));
        return ptr;
    }

    boost::shared_ptr<const char[]> _buffer;

    uint32_t _length;
};

/// @brief Swap two blobs
inline void swap(blob& src, blob& dst) BOND_NOEXCEPT
{
    src.swap(dst);
}

inline bool operator != (const blob& x, const blob& y) BOND_NOEXCEPT
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
    if (from._buffer.use_count() != 0)
    {
        boost::shared_array<char> ptr(const_cast<char*>(static_cast<const char*>(static_cast<const void*>(from._buffer.get()))),
                                      blob::deleter<boost::shared_ptr<const char[]> >(from._buffer));

        return T(ptr, static_cast<uint32_t>(0), from._length);
    }
    else
    {
        return T(from._buffer.get(), from._length);
    }
}

/// @brief Returns a blob with a copy of the data if the original one does not own the memory
/// (i.e. constructed using raw memory), and the same blob otherwise.
template <typename A>
inline blob blob_prolong(blob src, const A& allocator)
{
    if (src._buffer.use_count() != 0)
    {
        return src;
    }

    boost::shared_ptr<char[]> buffer = boost::allocate_shared_noinit<char[]>(allocator, src.length());
    ::memcpy(buffer.get(), src.content(), src.length());
    return blob(buffer, src.length());
}

inline blob blob_prolong(blob src)
{
    return blob_prolong(std::move(src), std::allocator<char>());
}


} // namespace bond
