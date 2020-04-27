// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/utility/enable_if.hpp>

#include <type_traits>


namespace bond
{
namespace detail
{

/// @brief Helper type that holds an allocator.
///
/// @remarks Applies empty-base-optimization when possible.
template <typename Alloc, typename Enable = void>
class allocator_holder;

template <typename Alloc>
using empty_base_eligible = std::integral_constant<bool,
    true
    #if __cplusplus >= 201402L
    && !std::is_final<Alloc>::value
    #endif
    && std::is_empty<Alloc>::value
    && std::is_copy_constructible<Alloc>::value>;

template <typename Alloc>
class allocator_holder<Alloc, typename boost::enable_if<empty_base_eligible<Alloc>>::type>
    : private Alloc
{
public:
    allocator_holder() = default;

    explicit allocator_holder(const Alloc& alloc) BOND_NOEXCEPT_IF(
        std::is_nothrow_copy_constructible<Alloc>::value)
        : Alloc{ alloc }
    {}

    const Alloc& get() const BOND_NOEXCEPT
    {
        return *this;
    }

    Alloc& get() BOND_NOEXCEPT
    {
        return *this;
    }
};

template <typename Alloc>
class allocator_holder<Alloc, typename boost::disable_if<empty_base_eligible<Alloc>>::type>
{
public:
    allocator_holder() = default;

    explicit allocator_holder(const Alloc& alloc) BOND_NOEXCEPT_IF(
        std::is_nothrow_copy_constructible<Alloc>::value)
        : _alloc{ alloc }
    {}

    const Alloc& get() const BOND_NOEXCEPT
    {
        return _alloc;
    }

    Alloc& get() BOND_NOEXCEPT
    {
        return _alloc;
    }

private:
    Alloc _alloc;
};
} // namespace detail
} // namespace bond
