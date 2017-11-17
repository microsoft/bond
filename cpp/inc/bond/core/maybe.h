// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <type_traits>
#include <utility>

namespace bond
{


BOND_NORETURN void NothingException();


/// @brief Type used for fields with default value of 'nothing'
/// See [User's Manual](../../manual/bond_cpp.html#default-value-of-nothing)
template <typename T>
class maybe
{
public:
    typedef T value_type;

    /// @brief Default constructor
    maybe()
        : _nothing(true)
    {}

    /// @brief Move constructor
    maybe(maybe&& that) BOND_NOEXCEPT_IF(std::is_nothrow_move_constructible<T>::value)
        : _value(std::move(that._value)),
          _nothing(std::move(that._nothing))
    {
        that._nothing = true;
    }

    template <typename Allocator>
    maybe(maybe&& that, const Allocator& alloc)
        : _value(std::move(that._value), alloc),
          _nothing(that._nothing)
    {
        that._nothing = true;
    }

    maybe(const maybe& that) = default;
    maybe& operator=(const maybe& that) = default;

    /// @brief Constructor from a value T
    explicit
    maybe(const T& value)
        : _value(value),
          _nothing(false)
    {
    }

    template <typename Allocator>
    explicit maybe(const Allocator& alloc)
        : _value(alloc),
          _nothing(true)
    {}

    template <typename Allocator>
    maybe(const maybe& that, const Allocator& alloc)
        : _value(that._value, alloc),
          _nothing(that._nothing)
    {}

    /// @brief Swap this object with that object
    void swap(maybe& that)
    {
        using std::swap;

        swap(_nothing, that._nothing);
        swap(_value, that._value);
    }

    /// @brief Check if this object contains nothing
    bool is_nothing() const
    {
        return _nothing;
    }

    /// @brief Set to default instance of T and return reference to that value
    T& set_value()
    {
        _nothing = false;
        return _value;
    }

    /// @brief Set to nothing
    void set_nothing()
    {
        _nothing = true;
    }

    /// @brief Reference to the value
    /// @throw NothingException if the object contains nothing
    T& value()
    {
        if (_nothing)
            NothingException();

        return _value;
    }

    /// @brief Constant reference to the value
    /// @throw NothingException if the object contains nothing
    const T& value() const
    {
        if (_nothing)
            NothingException();

        return _value;
    }

    /// @brief Compare for equality
    bool operator==(const maybe& that) const
    {
        return _nothing == that._nothing
            && (_nothing || _value == that._value);
    }

    bool operator!=(const maybe& that) const
    {
        return !(*this == that);
    }

    /// @brief Assign a value T
    maybe& operator=(const T& value)
    {
        _value = value;
        _nothing = false;
        return *this;
    }


    /// @brief Convert to constant reference to value
    /// @throw NothingException if the object contains nothing
    operator const T&() const
    {
        return value();
    }

private:
    T       _value;
    bool    _nothing;
};


template<typename T>
inline void swap(maybe<T>& x, maybe<T>& y)
{
    x.swap(y);
}

} // namespace bond
