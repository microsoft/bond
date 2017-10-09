// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "config.h"
#include "traits.h"
#include <boost/optional.hpp>
#include <boost/static_assert.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/core/ref.hpp>
#include <boost/intrusive_ptr.hpp>
#include <atomic>
#include <functional>


namespace bond
{
    /// @brief Non-sharable counter to be used with \ref capped_allocator.
    ///
    /// @tparam T underlying counter type.
    ///
    /// @tparam SingleThreaded flag to indicate thread safety.
    template <typename T = std::size_t, bool SingleThreaded = false>
    class capped_allocator_counter;

    /// @brief Shared counter to be used with \ref capped_allocator.
    ///
    /// @tparam T underlying counter type.
    ///
    /// @tparam SingleThreaded flag to indicate thread safety.
    template <typename T = std::size_t, bool SingleThreaded = false>
    class capped_allocator_shared_counter;

    /// @brief STL-compatible allocator adapter that limits the allocations.
    ///
    /// @tparam Alloc underlying allocator type.
    ///
    /// @tparam Counter underlying counter type.
    ///
    /// @remarks The provided counter is used to measure allocations in bytes.
    template <typename Alloc, typename Counter = capped_allocator_shared_counter<typename std::allocator_traits<Alloc>::size_type>>
    class capped_allocator;


    namespace detail
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

            value_or_reference(std::reference_wrapper<T> ref)
                : _value{},
                  _ref{ ref.get() }
            {}

            value_or_reference(boost::reference_wrapper<T> ref)
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

            T& get() const
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
            value_or_reference(T& value)
                : _ref{ value }
            {}

            value_or_reference(std::reference_wrapper<T> ref)
                : value_or_reference{ ref.get() }
            {}

            value_or_reference(boost::reference_wrapper<T> ref)
                : value_or_reference{ ref.get() }
            {}

            T& get() const
            {
                return _ref;
            }

        private:
            std::reference_wrapper<T> _ref;
        };


        /// @brief Helper type that holds an allocator.
        ///
        /// @remarks Applies empty-base-optimization when possible.
        template <typename Alloc, typename Enable = void>
        class allocator_holder : private Alloc
        {
        public:
            allocator_holder() = default;

            allocator_holder(const Alloc& alloc)
                : Alloc{ alloc }
            {}

            const Alloc& get() const
            {
                return *this;
            }

            Alloc& get()
            {
                return *this;
            }
        };

#if defined(_MSC_VER) && _MSC_VER < 1900
#pragma warning(push)
#pragma warning(disable: 4510)
#endif
        template <typename Alloc>
        class allocator_holder<Alloc, typename boost::disable_if<std::is_empty<Alloc>>::type>
        {
        public:
            allocator_holder() = default;

            allocator_holder(const Alloc& alloc)
                : _alloc{ alloc }
            {}

            const Alloc& get() const
            {
                return _alloc;
            }

            Alloc& get()
            {
                return _alloc;
            }

        private:
            Alloc _alloc;
        };
#if defined(_MSC_VER) && _MSC_VER < 1900
#pragma warning(pop)
#endif


        /// @brief Helper base class for counters.
        template <typename T>
        class capped_allocator_counter_base
        {
        public:
            BOOST_STATIC_ASSERT(std::is_integral<T>::value);

            using value_type = T;

            explicit capped_allocator_counter_base(T max_value) BOND_NOEXCEPT
                : _max_value{ max_value }
            {}

            capped_allocator_counter_base(const capped_allocator_counter_base& other) = delete;

            T max_value() const BOND_NOEXCEPT
            {
                return _max_value;
            }

        private:
            const T _max_value;
        };

    } // namespace detail


    template <typename T>
    class capped_allocator_counter<T, /*SingleThreaded*/ true> : public detail::capped_allocator_counter_base<T>
    {
    public:
#if !defined(_MSC_VER) || _MSC_VER >= 1900
        using detail::capped_allocator_counter_base<T>::capped_allocator_counter_base;
#else
        explicit capped_allocator_counter(T max_value) BOND_NOEXCEPT
            : detail::capped_allocator_counter_base<T>(max_value)
        {}
#endif

        bool try_add(T n) BOND_NOEXCEPT
        {
            if (n <= this->max_value() && _value <= this->max_value() - n)
            {
                _value += n;
                return true;
            }

            return false;
        }

        void subtract(T n) BOND_NOEXCEPT
        {
            BOOST_ASSERT(_value >= n);
            _value -= n;
        }

        T value() const BOND_NOEXCEPT
        {
            return _value;
        }

    private:
        T _value{};
    };


    template <typename T>
    class capped_allocator_counter<T, /*SingleThreaded*/ false> : public detail::capped_allocator_counter_base<T>
    {
    public:
#if !defined(_MSC_VER) || _MSC_VER >= 1900
        using detail::capped_allocator_counter_base<T>::capped_allocator_counter_base;
#else
        explicit capped_allocator_counter(T max_value) BOND_NOEXCEPT
            : detail::capped_allocator_counter_base<T>(max_value)
        {}
#endif

        bool try_add(T n) BOND_NOEXCEPT
        {
            if (n <= this->max_value())
            {
                const auto max_val = this->max_value() - n;

                for (auto val = _value.load(std::memory_order::memory_order_acquire); val <= max_val; )
                {
                    if (_value.compare_exchange_weak(
                            val,
                            val + n,
                            std::memory_order::memory_order_release,
                            std::memory_order::memory_order_acquire))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        void subtract(T n) BOND_NOEXCEPT
        {
            BOOST_ASSERT(value() >= n);
            _value.fetch_sub(n, std::memory_order::memory_order_relaxed);
        }

        T value() const BOND_NOEXCEPT
        {
            return _value.load(std::memory_order::memory_order_relaxed);
        }

    private:
        std::atomic<T> _value{};
    };


    template <typename T, bool SingleThreaded>
    class capped_allocator_shared_counter
    {
    public:
        using value_type = T;

        /// @brief Constructs counter using default \c "operator new".
        explicit capped_allocator_shared_counter(T max_value)
            : _value{ new counter{ max_value } }
        {}

        /// @brief Allocates counter using the provided allocator.
        ///
        /// @remarks The provided limit is applied to counter allocation as well.
        template <typename Alloc>
        static capped_allocator_shared_counter allocate(T max_value, const Alloc& alloc = {})
        {
            capped_allocator_counter<T, SingleThreaded> counter{ max_value };
            capped_allocator<Alloc, decltype(counter)&> capped_alloc{ std::ref(counter), alloc };
            capped_allocator_shared_counter shared_counter{
                max_value,
                capped_alloc }; // Only a copy of underlying \c Alloc will be stored as part of
                                // the counter, so it is safe to have the \c counter on the stack.
            shared_counter.try_add(counter.value());
            return shared_counter;
        }

        bool try_add(T val) BOND_NOEXCEPT
        {
            return _value->try_add(val);
        }

        void subtract(T val) BOND_NOEXCEPT
        {
            _value->subtract(val);
        }

        T max_value() const BOND_NOEXCEPT
        {
            return _value->max_value();
        }

        T value() const BOND_NOEXCEPT
        {
            return _value->value();
        }

    private:
        /// @brief Base counter type that is stored when default \c "operator new" is used.
        class counter : public capped_allocator_counter<T, SingleThreaded>
        {
        public:
#if !defined(_MSC_VER) || _MSC_VER >= 1900
            using capped_allocator_counter<T, SingleThreaded>::capped_allocator_counter;
#else
            explicit counter(T max_value) BOND_NOEXCEPT
                : capped_allocator_counter<T, SingleThreaded>(max_value)
            {}
#endif

            friend void intrusive_ptr_add_ref(counter* p) BOND_NOEXCEPT
            {
                ++p->_refs; // TODO: Use std::memory_order for atomic<T>
            }

            friend void intrusive_ptr_release(counter* p)
            {
                if (--p->_refs == 0)    // TODO: Use std::memory_order for atomic<T>
                {
                    p->delete_this();
                }
            }

        protected:
            virtual ~counter() = default;
            
            /// @brief Destroys and deallocates current instance.
            virtual void delete_this()
            {
                delete this;
            }

        private:
            typename std::conditional<SingleThreaded, T, std::atomic<T>>::type _refs{};
        };


        /// @brief Counter that is stored when using custom allocator.
        template <typename Alloc>
        class counter_with_allocator : public counter
        {
            using char_alloc = typename std::allocator_traits<Alloc>::template rebind_alloc<char>;

        public:
            /// @brief Allocates a counter by using a capped allocator.
            ///
            /// @remarks Only a copy of base \c Alloc is stored for further use.
            template <typename Counter>
            static counter* allocate_counter(T max_value, capped_allocator<Alloc, Counter>& capped_alloc)
            {
                capped_allocator<char_alloc, Counter> capped_char_alloc{ capped_alloc };
                return new (capped_char_alloc)                  // Allocate using capped allocator,
                    counter_with_allocator{
                        max_value,
                        capped_char_alloc.get_allocator() };    // but store only the base one.
            }

            /// @brief Deallocates memory when default \c "operator new" is used.
            ///
            /// @remarks This operator is needed because this is a polymorphic type and
            /// we are doing \c "delete this" in the base type which may call into this
            /// version if the runtime type happens to be \ref counter_with_allocator.
            static void operator delete(void* ptr)
            {
                // This is a private type and we never allocate it with default "operator new".
                // It is only allocated using the custom one that accepts a capped allocator.
                BOOST_ASSERT(false);
                ::operator delete(ptr);
            }

        private:
            counter_with_allocator(T max_value, const char_alloc& alloc)
                : counter{ max_value },
                  _alloc{ alloc }
            {}

            /// @brief Allocates counter using provided capped allocator.
            template <typename Counter>
            static void* operator new(std::size_t size, capped_allocator<char_alloc, Counter>& alloc)
            {
                BOOST_ASSERT(size == sizeof(counter_with_allocator));
                return alloc.allocate(size);
            }

            /// @brief Deallocates memory using provided capped allocator.
            ///
            /// @remarks This overload is called when corresponding allocation throws.
            template <typename Counter>
            static void operator delete(void* ptr, capped_allocator<char_alloc, Counter>& alloc)
            {
                operator delete(ptr, alloc.get_allocator());
            }

            /// @brief Helper operator to deallocate memory.
            static void operator delete(void* ptr, char_alloc& alloc)
            {
                return alloc.deallocate(static_cast<char*>(ptr), sizeof(counter_with_allocator));
            }

            /// @brief Destroys and deallocates current instance using stored allocator.
            void delete_this() override
            {
                auto alloc = _alloc;    // Save the allocators life because we will need it later.
                this->~counter_with_allocator();
                operator delete(this, alloc);
            }

            char_alloc _alloc;
        };


        /// @brief Allocates counter using provided capped allocator.
        ///
        /// @remarks Only a copy of base \c Alloc is stored for further use.
        template <typename Alloc, typename Counter>
        capped_allocator_shared_counter(T max_value, capped_allocator<Alloc, Counter>& capped_alloc)
            : _value{ counter_with_allocator<Alloc>::allocate_counter(max_value, capped_alloc) }
        {}

        boost::intrusive_ptr<counter> _value;
    };


    namespace detail
    {
        /// @brief Helper type to deal type that are not C++11 conformant.
        template <typename Alloc, typename Enable = void>
        struct allocator_reference_type_workaround
        {};

        /// @brief Transfer \c reference and \c const_reference types if available.
        template <typename Alloc>
        struct allocator_reference_type_workaround<Alloc, typename boost::enable_if_c<
            !std::is_void<typename Alloc::reference>::value
            && !std::is_void<typename Alloc::const_reference>::value>::type>
        {
            using reference = typename Alloc::reference;
            using const_reference = typename Alloc::const_reference;
        };

    } // namespace detail


    template <typename Alloc, typename Counter>
    class capped_allocator : private detail::allocator_holder<Alloc>, public detail::allocator_reference_type_workaround<Alloc>
    {
        using holder = detail::allocator_holder<Alloc>;
        using traits = std::allocator_traits<Alloc>;

    public:
        using value_type = typename traits::value_type;
        using pointer = typename traits::pointer;
        using const_pointer = typename traits::const_pointer;
        using void_pointer = typename traits::void_pointer;
        using const_void_pointer = typename traits::const_void_pointer;
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
        /// @param count instance of a \ref Counter, its reference or the max value to construct.
        ///
        /// @param alloc the base allocator instance.
        ///
        /// @param subtract_on_deallocate flag to indicate if counter must be adjust for deallocation.
        explicit capped_allocator(detail::value_or_reference<Counter> count, const Alloc& alloc = {}, bool subtract_on_deallocate = true)
            : holder{ alloc },
              _count{ count },
              _subtract_on_deallocate{ subtract_on_deallocate }
        {}

        /// @brief Constructs capped allocator adapter.
        ///
        /// @param counter_value max counter value.
        ///
        /// @param alloc the base allocator instance.
        ///
        /// @param subtract_on_deallocate flag to indicate if counter must be adjust for deallocation.
        ///
        /// @remarks The overload is used when \ref Counter can be allocated using \c Counter::allocate.
        template <typename C = Counter,
            typename boost::enable_if<check_method<C(*)(typename C::value_type, const Alloc&), &C::allocate>>::type* = nullptr>
        explicit capped_allocator(typename C::value_type counter_value, const Alloc& alloc = {}, bool subtract_on_deallocate = true)
            : capped_allocator{ Counter::allocate(counter_value, alloc), alloc, subtract_on_deallocate }
        {}

        /// @brief Converts from a compatible allocator.
        template <typename OtherAlloc, typename boost::enable_if<std::is_convertible<OtherAlloc, Alloc>>::type* = nullptr>
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
            return (std::min)(_count.get().max_value() / sizeof(value_type), traits::max_size(get_allocator()));
        }

        capped_allocator select_on_container_copy_construction()
        {
            return capped_allocator{ _count, traits::select_on_container_copy_construction(get_allocator()), _subtract_on_deallocate };
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
    inline bool operator==(const capped_allocator<Alloc, Counter>& a1, const capped_allocator<Alloc, Counter>& a2) BOND_NOEXCEPT
    {
        return a1.get_allocator() == a2.get_allocator();
    }

    template <typename Alloc, typename Counter>
    inline bool operator!=(const capped_allocator<Alloc, Counter>& a1, const capped_allocator<Alloc, Counter>& a2) BOND_NOEXCEPT
    {
        return a1.get_allocator() != a2.get_allocator();
    }

} // namespace bond
