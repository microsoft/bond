#pragma once

#include "unit_test_limits.h"
#include <bond/core/container_interface.h>
#include <array>
#include <boost/static_assert.hpp>

template <typename T>
struct SimpleList
{
    SimpleList()
        : size(0)
    {}
    
    SimpleList(SimpleList&& other)
        : size(std::move(other.size))
    {
        other.size = 0;
        for (uint32_t i = 0; i < size; ++i)
        {
            items[i] = std::move(other.items[i]);
        }
    }

    SimpleList(const SimpleList&) = default;
    SimpleList& operator=(const SimpleList&) = default;

    bool operator==(const SimpleList& rhs) const 
    {
        if (size != rhs.size)
            return false;

        for (uint32_t i = 0; i < size; ++i)
            if (items[i] != rhs.items[i])
                return false;
        
        return true;
    }
    
    T items[c_max_list_size];
    uint32_t size;
};


// list container interface and traits for SimpleList
namespace bond
{
    // SimpleList is a list container
    template <typename T>
    struct is_list_container<SimpleList<T> >
        : std::true_type {};


    // element_type trait
    template <typename T>
    struct element_type<SimpleList<T> >
    {
        typedef T type;
    };


    // enumerator
    template <typename T>
    class enumerator<SimpleList<T> >
        : boost::noncopyable
    {
    public:
        enumerator(SimpleList<T>& list)
            : list(list),
              index(0)
        {}

        bool more()
        {
            return index < list.size;
        }

        T& next()
        {
            return list.items[index++];
        }

    private:
        SimpleList<T>&  list;
        uint32_t        index;
    };


    // const_enumerator
    template <typename T>
    class const_enumerator<SimpleList<T> >
        : boost::noncopyable
    {
    public:
        const_enumerator(const SimpleList<T>& list)
            : list(list),
              index(0)
        {}

        bool more()
        {
            return index != list.size;
        }

        const T& next()
        {
            return list.items[index++];
        }

    private:
        const SimpleList<T>&    list;
        uint32_t                index;
    };
};


// container_size
template <typename T>
uint32_t container_size(const SimpleList<T>& list)
{
    return list.size;
}


// resize_list
template <typename T>
void resize_list(SimpleList<T>& list, uint32_t size)
{
    BOOST_ASSERT(size <= c_max_list_size);
    list.size = size;
}


namespace bond
{
    template <size_t N>
    struct is_string<std::array<char, N> >
        : std::true_type {};

    template <size_t N>
    struct is_string<const std::array<char, N> >
        : std::true_type {};

    template <size_t N>
    struct is_wstring<std::array<wchar_t, N> >
        : std::true_type {};

    template <size_t N>
    struct is_wstring<const std::array<wchar_t, N> >
        : std::true_type {};

    template <typename T, size_t N>
    struct element_type<std::array<T, N> >
    {
        typedef T type;
    };
}


namespace std
{
    template <typename T, size_t N>
    const T* string_data(const std::array<T, N>& str)
    {
        return &str[0];
    }

    template <typename T, size_t N>
    T* string_data(std::array<T, N>& str)
    {
        return &str[0];
    }

    template <size_t N>
    uint32_t string_length(const std::array<char, N>& str)
    {
        return static_cast<uint32_t>(strlen(&str[0]));
    }

    template <size_t N>
    uint32_t string_length(const std::array<wchar_t, N>& str)
    {
        return static_cast<uint32_t>(wcslen(&str[0]));
    }

    template<typename T, size_t N>
    void resize_string(std::array<T, N>& str, uint32_t size)
    {
        BOOST_ASSERT(size < N);
        str[size] = T(0);
    }
};


class ExtensibilityTest
{
public:
    static void Initialize();
    static void InitializeAssociative();
};
