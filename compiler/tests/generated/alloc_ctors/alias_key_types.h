
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0800
#error This file was generated by a newer version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0b00
#error This file was generated by an older version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>



namespace test
{
    
    struct foo
    {
        using allocator_type = arena;

        std::map<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, int32_t, std::less<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > >, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, int32_t> > > m;
        std::set<int32_t, std::less<int32_t>, typename std::allocator_traits<arena>::template rebind_alloc<int32_t> > s;
        
        struct _bond_vc12_ctor_workaround_ {};
        template <int = 0> // Workaround to avoid compilation if not used
        foo(_bond_vc12_ctor_workaround_ = {})
        {
        }

        
        // Compiler generated copy ctor OK
        foo(const foo&) = default;

        foo(const foo& other, const arena& allocator)
          : m(other.m, allocator),
            s(other.s, allocator)
        {
        }
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        foo(foo&& other)
          : m(std::move(other.m)),
            s(std::move(other.s))
        {
        }
#else
        foo(foo&&) = default;
#endif

        foo(foo&& other, const arena& allocator)
          : m(std::move(other.m), allocator),
            s(std::move(other.s), allocator)
        {
        }
        
        explicit
        foo(const arena& allocator)
          : m(allocator),
            s(allocator)
        {
        }
        
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        foo& operator=(foo other)
        {
            other.swap(*this);
            return *this;
        }
#else
        // Compiler generated operator= OK
        foo& operator=(const foo&) = default;
        foo& operator=(foo&&) = default;
#endif

        bool operator==(const foo& other) const
        {
            return true
                && (m == other.m)
                && (s == other.s);
        }

        bool operator!=(const foo& other) const
        {
            return !(*this == other);
        }

        void swap(foo& other)
        {
            using std::swap;
            swap(m, other.m);
            swap(s, other.s);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::test::foo& left, ::test::foo& right)
    {
        left.swap(right);
    }
} // namespace test
