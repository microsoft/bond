#pragma once

#include <cstdlib>


namespace examples
{
namespace scoped_allocator
{
    template <typename T>
    struct MyAllocator
    {
        using value_type = T;

        explicit MyAllocator(std::size_t /*unused*/)
        {}

        template <typename U>
        MyAllocator(const MyAllocator<U>& /*other*/)
        {}

        T* allocate(std::size_t n)
        {
            return static_cast<T*>(std::malloc(n * sizeof(T)));
        }

        void deallocate(T* p, std::size_t)
        {
            std::free(p);
        }
    };

    template <typename T>
    inline bool operator==(const MyAllocator<T>& /*first*/, const MyAllocator<T>& /*second*/)
    {
        return true;
    }

    template <typename T>
    inline bool operator!=(const MyAllocator<T>& /*first*/, const MyAllocator<T>& /*second*/)
    {
        return false;
    }

}
}
