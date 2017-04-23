#pragma once

#include "mpl.h"
#include <bond/core/config.h>
#include <boost/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/utility/enable_if.hpp>
#include <cstdint>
#include <type_traits>


namespace bond
{
namespace detail
{

template <template <typename> class TypeId, std::size_t Size>
class any
{
    BOOST_STATIC_ASSERT(Size >= sizeof(void*));

public:
    any() BOND_NOEXCEPT
        : _id{}
    {}

    any(const any& other)
    {
        emplace(other);
    }

    template <typename T>
    any(const T& value)
    {
        emplace(value);
    }

    ~any()
    {
        destroy();
    }

    any& operator=(const any& other)
    {
        return assign(other);
    }

    template <typename T>
    any& operator=(const T& value)
    {
        return assign(value);
    }

    bool empty() const BOND_NOEXCEPT
    {
        return _id == 0;
    }

    explicit operator bool() const BOND_NOEXCEPT
    {
        return !empty();
    }

    template <typename T>
    T* cast() BOND_NOEXCEPT
    {
        return TypeId<T>::value == _id ? functions::template table<T>::unsafe_cast(*this) : nullptr;
    }

    template <typename T>
    const T* cast() const BOND_NOEXCEPT
    {
        return TypeId<T>::value == _id ? functions::template table<T>::unsafe_cast(*this) : nullptr;
    }

    bool operator==(const any& other) const
    {
        return _id == other._id && (empty() || _functions.compare(*this, other));
    }

    bool operator!=(const any& other) const
    {
        return !(*this == other);
    }

private:
    using storage = typename std::aligned_storage<Size>::type;

    struct functions
    {
        void (*destroy)(any& x);

        void (*assign)(any& x, const any& other);

        void (*emplace)(any& x, const any& other);

        bool (*compare)(const any& x, const any& y);


        template <typename T, bool IsSmall>
        struct impl;

        template <typename T>
        struct impl<T, true>
        {
            static T* unsafe_cast(any& x) BOND_NOEXCEPT
            {
                BOOST_ASSERT(TypeId<T>::value == x._id);
                return reinterpret_cast<T*>(x.data());
            }

            static const T* unsafe_cast(const any& x) BOND_NOEXCEPT
            {
                BOOST_ASSERT(TypeId<T>::value == x._id);
                return reinterpret_cast<const T*>(x.data());
            }

            static void emplace(any& x, const T& value)
            {
                new (x.data()) T{ value };
            }

            static void destroy(any& x)
            {
                unsafe_cast(x)->~T();
            }
        };

        template <typename T>
        struct impl<T, false>
        {
            static T* unsafe_cast(any& x) BOND_NOEXCEPT
            {
                BOOST_ASSERT(TypeId<T>::value == x._id);
                return *reinterpret_cast<T**>(x.data());
            }

            static const T* unsafe_cast(const any& x) BOND_NOEXCEPT
            {
                BOOST_ASSERT(TypeId<T>::value == x._id);
                return *reinterpret_cast<const T* const*>(x.data());
            }

            static void emplace(any& x, const T& value)
            {
                *reinterpret_cast<T**>(x.data()) = new T{ value };
            }

            static void destroy(any& x)
            {
                delete unsafe_cast(x);
            }
        };

        template <typename T>
        struct table : impl<T, (sizeof(T) <= sizeof(storage))>
        {
            using base = impl<T, (sizeof(T) <= sizeof(storage))>;

            template <typename U = T, typename boost::enable_if<std::is_copy_assignable<U> >::type* = nullptr>
            static void assign(any& x, const T& value)
            {
                *base::unsafe_cast(x) = value;
            }

            template <typename U = T, typename boost::disable_if<std::is_copy_assignable<U> >::type* = nullptr>
            static void assign(any& x, const T& value)
            {
                // TODO: Cache allocated buffer and reuse.
                base::destroy(x);
                emplace(x, value);
            }

            static void emplace(any& x, const T& value)
            {
                base::emplace(x, value);
                x._id = TypeId<T>::value;  // Update the id if emplace did not throw.
            }

            static functions make() BOND_NOEXCEPT
            {
                return
                {
                    base::destroy,
                    [](any& x, const any& other) { assign(x, *base::unsafe_cast(other)); },
                    [](any& x, const any& other) { emplace(x, *base::unsafe_cast(other)); },
                    [](const any& x, const any& y) { return *base::unsafe_cast(x) == *base::unsafe_cast(y); }
                };
            }
        };
    };


    const void* data() const BOND_NOEXCEPT
    {
        return &_storage;
    }

    void* data() BOND_NOEXCEPT
    {
        return &_storage;
    }

    template <typename T>
    void emplace(const T& value)
    {
        functions::template table<T>::emplace(*this, value);
        _functions = functions::template table<T>::make();
    }

    void emplace(const any& other)
    {
        if (!other.empty())
        {
            other._functions.emplace(*this, other);
            _functions = other._functions;
        }
        else
        {
            _id = {};
        }
    }

    template <typename T>
    any& assign(const T& value)
    {
        if (!try_assign_same(value))
        {
            // TODO: Cache allocated buffer and reuse.
            destroy();
            emplace(value);
        }

        return *this;
    }

    template <typename T>
    bool try_assign_same(const T& value)
    {
        if (_id == TypeId<T>::value)
        {
            functions::template table<T>::assign(*this, value);
            return true;
        }

        return false;
    }

    bool try_assign_same(const any& other)
    {
        if (_id == other._id)
        {
            if (!empty())
            {
                _functions.assign(*this, other);
            }

            return true;
        }

        return false;
    }

    void destroy()
    {
        if (!empty())
        {
            _functions.destroy(*this);
            _id = {};
        }
    }


    std::uint32_t _id;
    storage _storage;
    functions _functions;
};


template <typename T, template <typename> class TypeId, std::size_t Size>
T* any_cast(any<TypeId, Size>* x) BOND_NOEXCEPT
{
    BOOST_ASSERT(x);
    return x->template cast<T>();
}

template <typename T, template <typename> class TypeId, std::size_t Size>
const T* any_cast(const any<TypeId, Size>* x) BOND_NOEXCEPT
{
    BOOST_ASSERT(x);
    return x->template cast<T>();
}


} // namespace detail

} // namespace bond
