// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "capped_allocator_fwd.h"

#include <boost/assert.hpp>
#include <boost/intrusive_ptr.hpp>

#include <atomic>
#include <functional>

namespace bond { namespace ext
{
    /// @brief Shared counter to be used with \ref capped_allocator.
    ///
    /// @tparam Counter underlying counter type.
    template <typename Counter>
    class shared_counter
    {
    public:
        using value_type = typename Counter::value_type;

        /// @brief Constructs counter using default \c "operator new".
        explicit shared_counter(value_type max_value)
            : _value{ new internal_counter{ max_value } }
        {}

        /// @brief Allocates counter using the provided allocator.
        ///
        /// @remarks The provided limit is applied to counter allocation as well.
        template <typename Alloc>
        shared_counter(value_type max_value, const Alloc& alloc)
            : shared_counter{ allocate_counter(max_value, alloc) }
        {}

        bool try_add(value_type val) BOND_NOEXCEPT
        {
            return _value->try_add(val);
        }

        void subtract(value_type val) BOND_NOEXCEPT
        {
            _value->subtract(val);
        }

        value_type max_value() const BOND_NOEXCEPT
        {
            return _value->max_value();
        }

        value_type value() const BOND_NOEXCEPT
        {
            return _value->value();
        }

    private:
        /// @brief Base counter type that is stored when default \c "operator new" is used.
        class internal_counter : public Counter
        {
        public:
            using Counter::Counter;

            friend void intrusive_ptr_add_ref(internal_counter* p) BOND_NOEXCEPT
            {
                ++p->_refs; // TODO: Use std::memory_order for atomic<T>
            }

            friend void intrusive_ptr_release(internal_counter* p)
            {
                if (--p->_refs == 0)    // TODO: Use std::memory_order for atomic<T>
                {
                    p->delete_this();
                }
            }

        protected:
            virtual ~internal_counter() = default;

            /// @brief Destroys and deallocates current instance.
            virtual void delete_this()
            {
                delete this;
            }

        private:
            typename std::conditional<
                Counter::is_thread_safe::value,
                std::atomic<value_type>,
                value_type>::type _refs{};
        };


        /// @brief Counter that is stored when using custom allocator.
        template <typename Alloc>
        class internal_counter_with_allocator : public internal_counter
        {
            using char_alloc = typename std::allocator_traits<Alloc>::template rebind_alloc<char>;

        public:
            /// @brief Allocates a counter by using a capped allocator.
            ///
            /// @remarks Only a copy of base \c Alloc is stored for further use.
            template <typename C>
            static internal_counter* allocate_counter(value_type max_value, capped_allocator<Alloc, C>& capped_alloc)
            {
                capped_allocator<char_alloc, C> capped_char_alloc{ capped_alloc };
                return new (capped_char_alloc)                  // Allocate using capped allocator,
                    internal_counter_with_allocator{
                        max_value,
                        capped_char_alloc.get_allocator() };    // but store only the base one.
            }

            /// @brief Deallocates memory when default \c "operator new" is used.
            ///
            /// @remarks This operator is needed because this is a polymorphic type and
            /// we are doing \c "delete this" in the base type which may call into this
            /// version if the runtime type happens to be \ref internal_counter_with_allocator.
            static void operator delete(void* /*ptr*/)
            {
                // This is a private type and we never allocate it with default "operator new".
                // It is only allocated using the custom one that accepts a capped allocator.
                BOOST_ASSERT(false);
            }

        private:
            internal_counter_with_allocator(value_type max_value, const char_alloc& alloc)
                : internal_counter{ max_value },
                  _alloc{ alloc }
            {}

            /// @brief Allocates counter using provided capped allocator.
            template <typename C>
            static void* operator new(std::size_t size, capped_allocator<char_alloc, C>& alloc)
            {
                BOOST_ASSERT(size == sizeof(internal_counter_with_allocator));
                return alloc.allocate(size);
            }

            /// @brief Deallocates memory using provided capped allocator.
            ///
            /// @remarks This overload is called when corresponding allocation throws.
            template <typename C>
            static void operator delete(void* ptr, capped_allocator<char_alloc, C>& alloc)
            {
                operator delete(ptr, alloc.get_allocator());
            }

            /// @brief Helper operator to deallocate memory.
            static void operator delete(void* ptr, char_alloc& alloc)
            {
                return alloc.deallocate(static_cast<char*>(ptr), sizeof(internal_counter_with_allocator));
            }

            /// @brief Destroys and deallocates current instance using stored allocator.
            void delete_this() override
            {
                auto alloc = _alloc;    // Make a copy of the allocator so we can use it later.
                this->~internal_counter_with_allocator();
                operator delete(this, alloc);
            }

            char_alloc _alloc;
        };


        /// @brief Allocates counter using provided capped allocator.
        ///
        /// @remarks Only a copy of base \c Alloc is stored for further use.
        template <typename Alloc, typename C>
        shared_counter(value_type max_value, capped_allocator<Alloc, C>& capped_alloc)
            : _value{ internal_counter_with_allocator<Alloc>::allocate_counter(max_value, capped_alloc) }
        {}

        /// @brief Allocates counter using the provided allocator.
        ///
        /// @remarks The provided limit is applied to counter allocation as well.
        template <typename Alloc>
        static shared_counter allocate_counter(value_type max_value, const Alloc& alloc)
        {
            single_threaded_counter<value_type> counter{ max_value };
            capped_allocator<Alloc, decltype(counter)&> capped_alloc{ std::ref(counter), alloc };
            shared_counter shared_counter{
                max_value,
                capped_alloc }; // Only a copy of underlying \c Alloc will be stored as part of
                                // the counter, so it is safe to have the \c counter on the stack.
            shared_counter.try_add(counter.value());
            return shared_counter;
        }

        boost::intrusive_ptr<internal_counter> _value;
    };

} } // namespace bond::ext

namespace std
{
    template <typename Counter, typename Alloc>
    struct uses_allocator<bond::ext::shared_counter<Counter>, Alloc> : std::true_type
    {};

} // namespace std
