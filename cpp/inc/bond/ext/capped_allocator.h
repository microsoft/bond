// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "capped_allocator_fwd.h"
#include "detail/value_or_reference.h"
#include "multi_threaded_counter.h"
#include "shared_counter.h"
#include "single_threaded_counter.h"

#include <bond/core/detail/alloc.h>

#include <boost/utility/enable_if.hpp>

namespace bond { namespace ext
{
    namespace detail
    {
        /// @brief Helper type to deal with types that are not C++11 conformant.
        template <typename Alloc, typename Enable = void>
        struct allocator_reference_type_workaround;

        /// @brief Transfer \c reference and \c const_reference types if available.
        template <typename Alloc>
        struct allocator_reference_type_workaround<Alloc, typename boost::enable_if_c<
            !std::is_void<typename Alloc::value_type>::value
            && !(std::is_void<typename Alloc::reference>::value
                || std::is_void<typename Alloc::const_reference>::value)>::type>
        {
            using reference = typename Alloc::reference;
            using const_reference = typename Alloc::const_reference;
        };

        /// @brief Use \c value_type if \c reference and \c const_reference are not available.
        template <typename Alloc>
        struct allocator_reference_type_workaround<Alloc, typename boost::enable_if_c<
            !std::is_void<typename Alloc::value_type>::value
            && (std::is_void<typename Alloc::reference>::value
                || std::is_void<typename Alloc::const_reference>::value)>::type>
        {
            using reference = typename Alloc::value_type&;
            using const_reference = const typename Alloc::value_type&;
        };

        /// @brief Use \c void for \c reference and \c const_reference in all other cases.
        template <typename Alloc>
        struct allocator_reference_type_workaround<Alloc, typename boost::enable_if<
            std::is_void<typename Alloc::value_type>>::type>
        {
            using reference = void;
            using const_reference = void;
        };

    } // namespace detail


    /// STL-compatible allocator adapter that fails allocations by throwing
    /// \c std::bad_alloc if the maximum number of bytes to be allocated is exceeded
    ///
    /// @tparam Alloc underlying allocator type.
    ///
    /// @tparam Counter underlying counter type.
    ///
    /// @remarks The provided counter is used to measure allocations in bytes.
    template <typename Alloc, typename Counter>
    class capped_allocator : private bond::detail::allocator_holder<Alloc>
    {
        using holder = typename capped_allocator::allocator_holder;
        using traits = std::allocator_traits<Alloc>;

    public:
        using value_type = typename traits::value_type;
        using pointer = typename traits::pointer;
        using const_pointer = typename traits::const_pointer;
        using void_pointer = typename traits::void_pointer;
        using const_void_pointer = typename traits::const_void_pointer;
        using reference = typename detail::allocator_reference_type_workaround<Alloc>::reference;
        using const_reference = typename detail::allocator_reference_type_workaround<Alloc>::const_reference;
        using size_type = typename traits::size_type;
        using difference_type = typename traits::difference_type;
        using propagate_on_container_copy_assignment = typename traits::propagate_on_container_copy_assignment;
        using propagate_on_container_move_assignment = typename traits::propagate_on_container_move_assignment;
        using propagate_on_container_swap = typename traits::propagate_on_container_swap;
#if __cplusplus >= 201703L
        using is_always_equal = typename traits::is_always_equal;
#endif

        template <typename U>
        struct rebind
        {
            using other = capped_allocator<typename traits::template rebind_alloc<U>, Counter>;
        };


        /// @brief Constructs capped allocator adapter.
        ///
        /// @param count counter value to use (see remarks for more
        /// details).
        ///
        /// @param alloc the base allocator instance.
        ///
        /// @param subtract_on_deallocate flag to indicate if counter must
        /// be adjusted for deallocation.
        ///
        /// @remarks When \p Counter is not a reference type, then \p count
        /// can be one of
        /// - an instance of \p Counter in which case it will be passed
        ///   by-value,
        /// - an instance of %std::ref/%boost::ref of a \p Counter in which
        ///   case only a reference will be held,
        /// - max value for the \p Counter which will be used to construct
        ///   one.
        ///
        /// When \p Counter is a reference type, then only a reference to an
        /// existing instance can be passed.
        explicit capped_allocator(
            detail::value_or_reference<Counter> count,
            const Alloc& alloc = {},
            bool subtract_on_deallocate = true)
            : holder{ alloc },
              _count{ count },
              _subtract_on_deallocate{ subtract_on_deallocate }
        {}

        /// @brief Constructs capped allocator adapter.
        ///
        /// @param max_value max counter value.
        ///
        /// @param alloc the base allocator instance.
        ///
        /// @param subtract_on_deallocate flag to indicate if counter must
        /// be adjusted for deallocation.
        ///
        /// @remarks The overload is used when \p Counter is
        /// allocator-aware.
        template <typename C = Counter,
            typename boost::enable_if<std::uses_allocator<C, Alloc>>::type* = nullptr>
        explicit capped_allocator(
            typename C::value_type max_value,
            const Alloc& alloc = {},
            bool subtract_on_deallocate = true)
            : capped_allocator{ Counter{ max_value, alloc }, alloc, subtract_on_deallocate }
        {}

        /// @brief Converts from a compatible allocator.
        template <typename OtherAlloc,
            typename boost::enable_if<std::is_convertible<OtherAlloc, Alloc>>::type* = nullptr>
        capped_allocator(const capped_allocator<OtherAlloc, Counter>& other)
            : capped_allocator{ other._count, other.get_allocator(), other._subtract_on_deallocate }
        {}

        pointer allocate(size_type n)
        {
            const auto size = n * sizeof(value_type);

            if (_count.get().try_add(size))
            {
                try
                {
                    return traits::allocate(get_allocator(), n);
                }
                catch (...)
                {
                    _count.get().subtract(size);
                    throw;
                }
            }

            throw std::bad_alloc{};
        }

        pointer allocate(size_type n, const_void_pointer hint)
        {
            const auto size = n * sizeof(value_type);

            if (_count.get().try_add(size))
            {
                try
                {
                    return traits::allocate(get_allocator(), n, hint);
                }
                catch (...)
                {
                    _count.get().subtract(size);
                    throw;
                }
            }

            throw std::bad_alloc{};
        }

        void deallocate(pointer ptr, size_type n)
        {
            traits::deallocate(get_allocator(), ptr, n);

            if (_subtract_on_deallocate)
            {
                _count.get().subtract(n * sizeof(value_type));
            }
        }

        template <typename T, typename... Args>
        void construct(T* ptr, Args&&... args)
        {
            traits::construct(get_allocator(), ptr, std::forward<Args>(args)...);
        }

        template <typename T>
        void destroy(T* ptr)
        {
            traits::destroy(get_allocator(), ptr);
        }

        size_type max_size() const BOND_NOEXCEPT
        {
            return (std::min)(
                _count.get().max_value() / sizeof(value_type),
                traits::max_size(get_allocator()));
        }

        capped_allocator select_on_container_copy_construction()
        {
            return capped_allocator{
                _count,
                traits::select_on_container_copy_construction(get_allocator()),
                _subtract_on_deallocate };
        }

        const Alloc& get_allocator() const BOND_NOEXCEPT
        {
            return holder::get();
        }

        Alloc& get_allocator() BOND_NOEXCEPT
        {
            return holder::get();
        }

        const Counter& get_counter() const BOND_NOEXCEPT
        {
            return _count.get();
        }

        Counter& get_counter() BOND_NOEXCEPT
        {
            return _count.get();
        }

    private:
        template <typename OtherAlloc, typename OtherCounter>
        friend class capped_allocator;

        detail::value_or_reference<Counter> _count;
        bool _subtract_on_deallocate;
    };


    template <typename Alloc, typename Counter>
    inline bool operator==(
        const capped_allocator<Alloc, Counter>& a1,
        const capped_allocator<Alloc, Counter>& a2) BOND_NOEXCEPT
    {
        return a1.get_allocator() == a2.get_allocator();
    }

    template <typename Alloc, typename Counter>
    inline bool operator!=(
        const capped_allocator<Alloc, Counter>& a1,
        const capped_allocator<Alloc, Counter>& a2) BOND_NOEXCEPT
    {
        return a1.get_allocator() != a2.get_allocator();
    }

} } // namespace bond::ext
