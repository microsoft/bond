// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/alloc.h"
#include "traits.h"

#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/utility/enable_if.hpp>

#include <memory>
#include <type_traits>
#include <utility>

namespace bond
{


[[noreturn]] void NothingException();

namespace detail
{

/// Internal base class with shared implementation between the two maybe
/// variants. Consult the documentation for bond::maybe<T> for its public
/// interface.
template <typename T>
class maybe_common
{
public:
    /// @brief The type of the value that may be inside the maybe.
    using value_type = T;

    maybe_common() = default;

    maybe_common(const maybe_common&) = default;

    template <typename... Args>
    explicit maybe_common(const T& value, Args&&... args)
    {
        _value.emplace(value, std::forward<Args>(args)...);
    }

    template <typename... Args>
    explicit maybe_common(T&& value, Args&&... args)
    {
        _value.emplace(std::move(value), std::forward<Args>(args)...);
    }

    maybe_common(maybe_common&& that) BOND_NOEXCEPT_IF(std::is_nothrow_move_constructible<boost::optional<T>>::value)
        : _value(std::move(that._value))
    {
        // unlike std::optional/boost::optional, moved-from bond::maybe
        // instances are guaranteed to be nothing.
        //
        // asigning boost::none is noexcept, but assigning { } is not
        that._value = boost::none;
    }

    /// @brief Check if this object contains nothing.
    /// @return true if this holds nothing; otherwise false.
    bool is_nothing() const BOND_NOEXCEPT
    {
        return !static_cast<bool>(_value);
    }

    /// @brief Check if this object contains a value.
    /// @return true if this object holds a value; otherwise false.
    /// @since 8.0.0
    explicit operator bool() const BOND_NOEXCEPT
    {
        return !is_nothing();
    }

    /// @brief Set to nothing.
    void set_nothing() BOND_NOEXCEPT
    {
        // asigning boost::none is noexcept, but assigning { } is not
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
    /// @throw CoreException if the object contains nothing
    T& value()
    {
        if (is_nothing())
        {
            NothingException();
        }

        return *_value;
    }

    /// @brief Get a constant reference to the value.
    /// @throw CoreException if the object contains nothing
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

    /// @brief Assign from another maybe.
    maybe_common& operator=(const maybe_common&) = default;
    /// @brief Move assign from another maybe.
    maybe_common& operator=(maybe_common&&) = default;

    /// @brief Compares a maybe and a value for equality.
    ///
    /// @return false if the maybe holds nothing; otherwise, calls
    /// operator== with the maybe's value and the provided value.
    ///
    /// @since 8.0.0
    friend bool operator==(const maybe_common& lhs, const T& rhs)
    {
        return lhs._value == rhs;
    }

    /// @brief Compares a maybe and a value for inequality.
    ///
    /// See operator==(const maybe_common&,const T&) for details about how
    /// maybes holding nothing are handled.
    ///
    /// @since 8.0.0
    friend bool operator!=(const maybe_common& lhs, const T& rhs)
    {
        return !(lhs == rhs);
    }

    /// @brief Compares a value and a maybe for equality.
    ///
    /// @return false if the maybe holds nothing; otherwise, calls
    /// operator== with the provided value and maybe's value.
    ///
    /// @since 8.0.0
    friend bool operator==(const T& lhs, const maybe_common& rhs)
    {
        return lhs == rhs._value;
    }

    /// @brief Compares and a value and a maybe for inequality.
    ///
    /// See operator==(const T&,const maybe_common&) for details about how
    /// maybes holding nothing are handled.
    ///
    /// @since 8.0.0
    friend bool operator!=(const T& lhs, const maybe_common& rhs)
    {
        return !(lhs == rhs);
    }

protected:
    boost::optional<T> _value;
};


template <typename T, typename Enable = void> struct
has_allocator
    : std::false_type {};

template <typename T> struct
has_allocator<T, typename boost::enable_if<std::is_class<typename T::allocator_type> >::type>
    : std::true_type {};

} // namespace detail


template <typename T, typename Enabled = void>
class maybe;

/// @brief Type used for fields with default values of \c nothing.
///
/// This specialization is used for instance of T without allocators.
///
/// See the [User's
/// Manual](../../manual/bond_cpp.html#default-value-of-nothing) for more
/// details about default values of \c nothing.
///
/// @see For details of %maybe's comparison operators, see
/// \li operator==(const detail::maybe_common&,const detail::maybe_common&)
/// \li operator!=(const detail::maybe_common&,const detail::maybe_common&)
/// \li operator==(const detail::maybe_common&,const T&)
/// \li operator==(const T&,const detail::maybe_common&)
/// \li operator!=(const detail::maybe_common&,const T&)
/// \li operator!=(const T&,const detail::maybe_common&)
template <typename T>
class maybe<T, typename boost::disable_if<detail::has_allocator<T> >::type>
    : public detail::maybe_common<T>
{
public:
    /// @brief Create a maybe that holds nothing.
    maybe() = default;

    /// @brief Copy a maybe
    maybe(const maybe&) = default;
    /// @brief Move a maybe.
    ///
    /// @note Unlike \c std::optional, a moved-from maybe holds nothing
    /// (compared to a moved-from T).
    maybe(maybe&&) = default;

    /// @brief Create a maybe that holds a value by copying \c value.
    explicit
    maybe(const T& value)
        : detail::maybe_common<T>(value)
    { }

    /// @brief Create a maybe that holds a value by moving from \c value.
    ///
    /// @since 8.0.0
    explicit
    maybe(T&& value)
        : detail::maybe_common<T>(std::move(value))
    { }

    maybe& operator=(const maybe&) = default;
    maybe& operator=(maybe&&) = default;

    /// @brief Assign by copying a value.
    maybe& operator=(const T& value)
    {
        this->emplace(value);
        return *this;
    }

    /// @brief Move-assign from a value.
    /// @since 8.0.0
    maybe& operator=(T&& value)
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
    friend bool operator==(const maybe& lhs, const maybe& rhs)
    {
        return lhs._value == rhs._value;
    }

    /// @brief Compares two maybes for value inequality.
    ///
    /// See operator==(const maybe_common&,const maybe_common&) for details
    /// about how maybes holding nothing are handled.
    ///
    friend bool operator!=(const maybe& lhs, const maybe& rhs)
    {
        return lhs._value != rhs._value;
    }

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

    /// @brief Swap this object with \c that.
    void swap(maybe& that)
    {
        using std::swap;
        swap(this->_value, that._value);
    }
};

/// @brief Type used for fields with default values of \c nothing.
///
/// This specialization is used for instances of T with allocators.
///
/// See the [User's
/// Manual](../../manual/bond_cpp.html#default-value-of-nothing) for more
/// details about default values of \c nothing.
///
/// @see For details of %maybe's comparison operators, see
/// \li operator==(const detail::maybe_common&,const detail::maybe_common&)
/// \li operator!=(const detail::maybe_common&,const detail::maybe_common&)
/// \li operator==(const detail::maybe_common&,const T&)
/// \li operator==(const T&,const detail::maybe_common&)
/// \li operator!=(const detail::maybe_common&,const T&)
/// \li operator!=(const T&,const detail::maybe_common&)
template <typename T>
class maybe<T, typename boost::enable_if<detail::has_allocator<T> >::type>
    : public detail::maybe_common<T>,
      private detail::allocator_holder<typename T::allocator_type>
{
    using alloc_holder = detail::allocator_holder<typename T::allocator_type>;

public:
    // allocator_holder may inherit from an allocator that has its own
    // value_type. Add an using to explicitly "export" the one from
    // maybe_common
    using typename detail::maybe_common<T>::value_type;

    /// @brief The type of the allocator in use.
    using allocator_type = typename T::allocator_type;

    maybe() = default;
    /// @brief Copy a maybe
    maybe(const maybe&) = default;
    /// @brief Move a maybe.
    ///
    /// @note Unlike \c std::optional, a moved-from maybe holds nothing
    /// (compared to a moved-from T).
    maybe(maybe&&) = default;

    /// @brief Allocator-extended copy constructor. Uses alloc as the new
    /// allocator, makes a copy of \c that.
    maybe(const maybe& that, const allocator_type& alloc)
        : detail::maybe_common<T>(),
          alloc_holder(alloc)
    {
        if (!that.is_nothing())
        {
            this->emplace(*that._value, alloc);
        }
    }

    /// @brief Allocator-extended move constructor. Uses alloc as the new
    /// allocator, makes moved from \c that.
    ///
    /// @note Unlike \c std::optional, a moved-from maybe holds nothing
    /// (compared to a moved-from T).
    maybe(maybe&& that, const allocator_type& alloc)
        : detail::maybe_common<T>(),
          alloc_holder(alloc)
    {
        if (!that.is_nothing())
        {
            this->emplace(std::move(*that._value), alloc);

            // asigning boost::none is noexcept, but assigning { } is not
            that._value = boost::none;
        }
    }

    /// @brief Construct a maybe that holds nothing, but remember the
    /// allocator so that it can be used to construct a T if needed.
    ///
    /// @since 8.0.0
    explicit maybe(const allocator_type& alloc)
        : detail::maybe_common<T>(),
          alloc_holder(alloc)
    { }

    /// @brief Create a maybe that holds a copy of \c value.
    explicit maybe(const T& value)
        : detail::maybe_common<T>(value),
          alloc_holder()
    { }

    /// @brief Create a maybe that holds a value by moving from \c value.
    ///
    /// @since 8.0.0
    maybe(T&& value)
        : detail::maybe_common<T>(std::move(value)),
          alloc_holder()
    { }

    maybe& operator=(const maybe&) = default;
    maybe& operator=(maybe&&) = default;

    /// @brief Assign by copying \c value.
    maybe& operator=(const T& value)
    {
        this->emplace(value);
        return *this;
    }

    /// @brief Move-assign from \c value.
    /// @since 8.0.0
    maybe& operator=(T&& value)
    {
        this->emplace(std::move(value));
        return *this;
    }

    // We need to get rid of any operator== that may come from the
    // allocator_holder so the friend free functions from maybe_common don't
    // have any competition.
    bool operator==(const alloc_holder&) = delete;

    /// @brief Compares two maybes for value equality.
    ///
    /// @return true if both maybes hold nothing; returns false if one maybe
    /// holds nothing and the other holds a values; otherwise, calls
    /// operator== with the two values.
    ///
    friend bool operator==(const maybe& lhs, const maybe& rhs)
    {
        return lhs._value == rhs._value;
    }

    /// @brief Compares two maybes for value inequality.
    ///
    /// See operator==(const maybe_common&,const maybe_common&) for details
    /// about how maybes holding nothing are handled.
    ///
    friend bool operator!=(const maybe& lhs, const maybe& rhs)
    {
        return lhs._value != rhs._value;
    }

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
            this->emplace(base_alloc_holder().get());
        }

        return *this->_value;
    }

    /// @brief Swap this object with \c that.
    void swap(maybe& that)
    {
        using std::swap;
        swap(this->_value, that._value);
        swap(base_alloc_holder(), that.base_alloc_holder());
    }

    /// @brief Get the allocator that this maybe uses.
    /// @since 8.0.0
    allocator_type get_allocator() const BOND_NOEXCEPT
    {
        return base_alloc_holder().get();
    }

private:
    detail::maybe_common<T>& base_common() BOND_NOEXCEPT { return *this; }
    const detail::maybe_common<T>& base_common() const BOND_NOEXCEPT { return *this; }
    alloc_holder& base_alloc_holder() BOND_NOEXCEPT { return *this; }
    const alloc_holder& base_alloc_holder() const BOND_NOEXCEPT { return *this; }
};

/// @brief Swap two maybes.
template<typename T>
inline void swap(maybe<T>& x, maybe<T>& y)
{
    x.swap(y);
}

} // namespace bond
