// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include <bond/core/traits.h>

#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/utility/typed_in_place_factory.hpp>

#include <memory>
#include <type_traits>
#include <utility>

namespace bond
{


BOND_NORETURN void NothingException();

namespace detail
{

template <typename T>
class maybe_common
{
public:
    /// @brief The type of the value that may be inside the maybe.
    using value_type = T;

    /// @brief Check if this object contains nothing.
    /// @return true if this holds nothing; otherwise false.
    bool is_nothing() const BOND_NOEXCEPT
    {
        return !static_cast<bool>(_value);
    }

    /// @brief Check if this object contains a value.
    /// @return true if this object hold a value; otherwise false.
    /// @since 8.0.0
    explicit operator bool() const BOND_NOEXCEPT
    {
        return !is_nothing();
    }

    /// @brief Set to nothing.
    void set_nothing()
    {
        _value = boost::none;
    }

    /// @brief Construct a value in place.
    ///
    /// @since 8.0.0
    template<typename... Args>
    T& emplace(Args&&... args)
    {
        _value.emplace(std::forward<Args>(args)...);
        return *_value;
    }

    /// @brief Get a reference to the value.
    /// @throw NothingException if the object contains nothing
    T& value()
    {
        if (is_nothing())
        {
            NothingException();
        }

        return *_value;
    }

    /// @brief Get a constant reference to the value.
    /// @throw NothingException if the object contains nothing
    const T& value() const
    {
        if (is_nothing())
        {
            NothingException();
        }

        return *_value;
    }

    /// @brief Get a reference to the value.
    ///
    /// Will never throw, but has undefined behavior if the object contains
    /// nothing.
    ///
    /// @since 8.0.0
    T& value(const std::nothrow_t&) BOND_NOEXCEPT
    {
        BOOST_ASSERT(!is_nothing());
        return *_value;
    }

    /// @brief Get a constant reference to the value.
    ///
    /// Will never throw, but has undefined behavior if the object contains
    /// nothing.
    ///
    /// @since 8.0.0
    const T& value(const std::nothrow_t&) const BOND_NOEXCEPT
    {
        BOOST_ASSERT(!is_nothing());
        return *_value;
    }

protected:
    maybe_common() = default;
    maybe_common(const maybe_common&) = default;

    explicit maybe_common(const T& value)
        : _value(value)
    { }

    template <typename Alloc>
    maybe_common(const T& value, const Alloc& alloc)
        : _value(boost::in_place<T>(value, alloc))
    { }

    maybe_common(maybe_common&& that) BOND_NOEXCEPT_IF(std::is_nothrow_move_constructible<boost::optional<T>>::value)
        : _value(std::move(that._value))
    {
        // unlike std::optional/boost::optional, moved-from bond::maybe
        // instances are guaranteed to be nothing.
        that._value = { };
    }

    maybe_common& operator=(const maybe_common&) = default;

    /// @brief Assign by copying the value.
    maybe_common& operator=(const T& value)
    {
        this->emplace(value);
        return *this;
    }

    /// @brief Move-assign from value.
    /// @since 8.0.0
    maybe_common& operator=(T&& value)
    {
        this->emplace(std::move(value));
        return *this;
    }

    /// @brief Compares two maybes for value equality.
    ///
    /// @return true if both maybes hold nothing; returns false if one maybe
    /// holds nothing and the other holds a values; otherwise, calls
    /// operator== with the two values.
    ///
    /// @since 8.0.0
    bool operator==(const maybe_common& that) const
    {
        return this->_value == that._value;
    }

    /// @brief Compares two maybes for value inequality.
    ///
    /// See operator==(const maybe_common&) for details about how maybes holding
    /// nothing are handled.
    ///
    /// @since 8.0.0
    bool operator!=(const maybe_common& that) const
    {
        return !(*this == that);
    }

    /// @brief Compares a maybe and a value for equality.
    ///
    /// @return false if the maybe holds nothing; otherwise, calls
    /// operator== with maybe's value and the provided value.
    ///
    /// @since 8.0.0
    bool operator==(const T& that) const
    {
        return this->_value == that;
    }

    /// @brief Compares a maybe and a value for inequality.
    ///
    /// See operator==(const T&) for details about how maybes holding
    /// nothing are handled.
    ///
    /// @since 8.0.0
    bool operator!=(const T& that) const
    {
        return !(*this == that);
    }

    boost::optional<T> _value;
};

} // namespace detail

template <typename T, typename Enabled = void>
class maybe;

/// @brief Type used for fields with default value of 'nothing'
///
/// This specialization is used for instance of T without allocators.
///
/// See [User's Manual](../../manual/bond_cpp.html#default-value-of-nothing)
template <typename T>
class maybe<T, typename boost::disable_if<detail::has_allocator<T>>::type>
    : public detail::maybe_common<T>
{
public:
    /// @brief Create a maybe that holds nothing.
    maybe() = default;

    maybe(const maybe& that) = default;

    maybe(maybe&& that) BOND_NOEXCEPT_IF(std::is_nothrow_move_constructible<typename detail::maybe_common<T>>::value)
        : detail::maybe_common<T>(std::move(that))
    { }

    /// @brief Create a maybe that holds a value by copying the value.
    explicit
    maybe(const T& value)
        : detail::maybe_common<T>(value)
    { }

    /// @brief Create a maybe that holds a value by moving from the value.
    ///
    /// @since 8.0.0
    explicit
    maybe(T&& value)
        : detail::maybe_common<T>(std::move(value))
    { }

    maybe& operator=(const maybe&) = default;
    using detail::maybe_common<T>::operator=;

    /// @brief Set the maybe to hold a value, if needed.
    ///
    /// If this instance contains nothing, construct a default instance of
    /// T; otherwise, preserve the existing value.
    ///
    /// @return A reference to the value.
    T& set_value()
    {
        if (this->is_nothing())
        {
            this->emplace();
        }

        return *this->_value;
    }

    /// @brief Swap this object with that object
    void swap(maybe& that)
    {
        using std::swap;
        swap(this->_value, that._value);
    }

    using detail::maybe_common<T>::operator==;
    using detail::maybe_common<T>::operator!=;

};

/// @brief Type used for fields with default value of 'nothing'
///
/// This specialization is used for instance of T with allocators.
///
/// See [User's Manual](../../manual/bond_cpp.html#default-value-of-nothing)
template <typename T>
class maybe<T, typename boost::enable_if<detail::has_allocator<T>>::type>
    : public detail::maybe_common<T>,
      private detail::allocator_type<T>::type
{
public:
    using allocator_type = typename detail::allocator_type<T>::type;

    /// @brief Create a  maybe that holds nothing.
    maybe() = default;
    maybe(const maybe&) = default;

    maybe(const maybe& that, const allocator_type& alloc)
        : detail::maybe_common<T>(),
          allocator_type(alloc)
    {
        if (that._value)
        {
            this->emplace(*that._value, alloc);
        }
    }

    maybe(maybe&& that) BOND_NOEXCEPT_IF(std::is_nothrow_move_constructible<typename detail::maybe_common<T>>::value
                                         && std::is_nothrow_move_constructible<allocator_type>::value)
        : detail::maybe_common<T>(std::move(that.base())),
          allocator_type(std::move(that.allocator()))
    { }

    maybe(maybe&& that, const allocator_type& alloc)
        : detail::maybe_common<T>(),
          allocator_type(alloc)
    {
        if (that._value)
        {
            this->emplace(std::move(*that._value), alloc);
        }

        // unlike std::optional/boost::optional, moved-from bond::maybe
        // instances are guaranteed to be nothing.
        that._value = { };
    }

    /// @brief Construct a maybe that holds nothing, but remember the
    /// allocator so that it can be used to construct a T if needed.
    ///
    /// @since 8.0.0
    explicit maybe(const allocator_type& alloc)
        : detail::maybe_common<T>(),
          allocator_type(alloc)
    { }

    /// @brief Create a maybe that holds a copy of the value.
    explicit maybe(const T& value)
        : detail::maybe_common<T>(value),
          allocator_type()
    { }

    /// @since 8.0.0
    maybe(const T& value, const allocator_type& alloc)
        : detail::maybe_common<T>(value, alloc),
        allocator_type(alloc)
    { }

    /// @brief Create a non-empty maybe by moving from the value.
    ///
    /// @since 8.0.0
    maybe(T&& value)
        : detail::maybe_common<T>(std::move(value)),
        allocator_type()
    { }

    /// @since 8.0.0
    maybe(T&& value, const allocator_type& alloc)
        : detail::maybe_common<T>(),
          allocator_type(alloc)
    {
        this->emplace(std::move(value), alloc);
    }

    maybe& operator=(const maybe&) = default;
    using detail::maybe_common<T>::operator=;

    /// @brief Set to non-empty, if needed.
    ///
    /// If this object contains nothing, construct an instance of T, passing
    /// it the saved allocator; otherwise, preserve the existing value.
    ///
    /// @return A reference to the value.
    T& set_value()
    {
        if (this->is_nothing())
        {
            this->emplace(allocator());
        }

        return *this->_value;
    }

    /// @brief Swap this object with that object
    void swap(maybe& that)
    {
        using std::swap;
        swap(this->_value, that._value);
        swap(allocator(), that.allocator());
    }

    /// @brief Get the allocator that this maybe uses.
    /// @since 8.0.0
    allocator_type get_allocator() const BOND_NOEXCEPT
    {
        return allocator();
    }

    using detail::maybe_common<T>::operator==;
    using detail::maybe_common<T>::operator!=;

private:
    detail::maybe_common<T>& base() BOND_NOEXCEPT
    {
        return static_cast<detail::maybe_common<T>&>(*this);
    }

    const detail::maybe_common<T>& base() const BOND_NOEXCEPT
    {
        return static_cast<const detail::maybe_common<T>&>(*this);
    }

    allocator_type& allocator() BOND_NOEXCEPT
    {
        return static_cast<allocator_type&>(*this);
    }

    const allocator_type& allocator() const BOND_NOEXCEPT
    {
        return static_cast<const allocator_type&>(*this);
    }
};

template<typename T>
inline void swap(maybe<T, void>& x, maybe<T, void>& y)
{
    x.swap(y);
}

} // namespace bond
