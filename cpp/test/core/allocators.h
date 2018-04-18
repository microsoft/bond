#pragma once

#include <memory>
#include <type_traits>
#include <utility>

namespace detail
{

template<typename T = char>
class TestAllocator
{
public:
    typedef typename std::remove_const<T>::type value_type;
    typedef value_type* pointer;
    typedef value_type& reference;
    typedef const value_type* const_pointer;
    typedef const value_type& const_reference;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;

    TestAllocator()
    {}

    template<typename U>
    TestAllocator(const TestAllocator<U>&)
    {}

    template<class Other>
    struct rebind
    {
        // convert this type to TestAllocator<Other>
        typedef TestAllocator<Other> other;
    };

    bool operator==(const TestAllocator&) const
    {
        return true;
    }

    bool operator!=(const TestAllocator&) const
    {
        return false;
    }

    pointer address(reference val) const
    {
        return &val;
    }

    const_pointer address(const_reference val) const
    {
        return &val;
    }

    void deallocate(pointer ptr, size_type)
    {
        free(ptr);
    }

    pointer allocate(size_type count)
    {
        if (count < 0xffffffff)
            return static_cast<T*>(malloc(sizeof(T)* count));
        else
            throw std::bad_alloc();
    }

    pointer allocate(size_type count, const void*)
    {
        // allocate array of count elements, ignore hint
        return allocate(count);
    }

    void construct(pointer ptr, const T& val)
    {
        // construct object at ptr with value val
        new((void*)ptr) T(val);
    }

#if !defined(BOND_NO_CXX11_ALLOCATOR)
    template <typename U, typename ...Args>
    void construct(U* ptr, Args&&...args)
    {
        new ((void*)ptr) (U)(std::forward<Args>(args)...);
    }
#endif

    void destroy(pointer ptr)
    {
        (void)ptr;
        // destroy object at ptr
        ptr->~T();
    }

    size_type max_size() const
    {
        // estimate maximum array size
        size_type count = static_cast<size_type>(-1) / sizeof(T);
        return count > 0 ? count : 1;
    }
};

}

typedef detail::TestAllocator<> TestAllocator;

template <typename T = char>
struct allocator_with_state : std::allocator<T>
{
    template <typename U>
    struct rebind
    {
        using other = allocator_with_state<U>;
    };

    template <typename U>
    allocator_with_state(std::shared_ptr<U> state)
        : state{ std::move(state) }
    {}

    template <typename U>
    allocator_with_state(const allocator_with_state<U>& other)
        : state{ other.state }
    {}

    std::shared_ptr<void> state;
};

template <typename T>
inline bool operator==(const allocator_with_state<T>& a1, const allocator_with_state<T>& a2)
{
    return a1.state == a2.state;
}

template <typename T>
inline bool operator!=(const allocator_with_state<T>& a1, const allocator_with_state<T>& a2)
{
    return !(a1 == a2);
}
