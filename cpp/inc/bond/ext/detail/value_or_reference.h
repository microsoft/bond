// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/core/ref.hpp>
#include <boost/optional.hpp>
#include <boost/utility/enable_if.hpp>

#include <functional>
#include <type_traits>

namespace bond { namespace ext { namespace detail
{
    /// @brief Helper type that can hold either a value or a reference.
    template <typename T>
    class value_or_reference
    {
    public:
        template <typename U = T, typename boost::enable_if<std::is_constructible<T, U&&>>::type* = nullptr>
        value_or_reference(U&& value = {})
            : _value{ std::forward<U>(value) },
              _ref{ *_value }
        {}

        value_or_reference(std::reference_wrapper<T> ref) BOND_NOEXCEPT
            : _value{},
              _ref{ ref.get() }
        {}

        value_or_reference(boost::reference_wrapper<T> ref) BOND_NOEXCEPT
            : _value{},
              _ref{ ref.get() }
        {}

        value_or_reference(const value_or_reference& other)
            : _value{ other._value },
              _ref{ _value ? std::ref(*_value) : other._ref }
        {}

        value_or_reference& operator=(const value_or_reference& other)
        {
            _value = other._value;
            _ref = _value ? std::ref(*_value) : other._ref;
            return *this;
        }

        T& get() const BOND_NOEXCEPT
        {
            return _ref;
        }

    private:
        template <typename U>
        friend class value_or_reference;

        boost::optional<T> _value;
        std::reference_wrapper<T> _ref;
    };


    template <typename T>
    class value_or_reference<T&>
    {
    public:
        value_or_reference(T& value) BOND_NOEXCEPT
            : _ref{ value }
        {}

        value_or_reference(std::reference_wrapper<T> ref) BOND_NOEXCEPT
            : value_or_reference{ ref.get() }
        {}

        value_or_reference(boost::reference_wrapper<T> ref) BOND_NOEXCEPT
            : value_or_reference{ ref.get() }
        {}

        T& get() const BOND_NOEXCEPT
        {
            return _ref;
        }

    private:
        std::reference_wrapper<T> _ref;
    };

} } } // namespace bond::ext::detail
