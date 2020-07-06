
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0901
#error This file was generated by a newer version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0c10
#error This file was generated by an older version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>



namespace test
{
    template <typename T>
    using List = std::list<T, typename std::allocator_traits<arena>::template rebind_alloc<T> >;

    template <typename T>
    using Vector = std::vector<T, typename std::allocator_traits<arena>::template rebind_alloc<T> >;

    template <typename T>
    using Set = std::set<T, std::less<T>, typename std::allocator_traits<arena>::template rebind_alloc<T> >;

    template <typename K, typename T>
    using Map = std::map<K, T, std::less<K>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const K, T> > >;

    using String = std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >;

    using NestedAliases = ::test::Set< ::test::List< ::test::Map<int32_t, ::test::String> > >;

    using TheFoo = ::test::foo;

    
    struct foo
    {
        using allocator_type = arena;

        ::test::List<bool> l;
        ::test::Vector<bool> v;
        ::test::Set<bool> s;
        ::test::Map< ::test::String, bool> m;
        ::test::String st;
        ::test::String d;
        ::bond::maybe< ::test::List<bool> > l1;
        ::bond::maybe< ::test::Vector<bool> > v1;
        ::bond::maybe< ::test::Set<bool> > s1;
        ::bond::maybe< ::test::Map< ::test::String, bool> > m1;
        ::bond::maybe< ::test::String> st1;
        ::test::NestedAliases na;
        
        template <int = 0> // Workaround to avoid compilation if not used
        foo()
          : d("foo")
        {
        }

        
        // Compiler generated copy ctor OK
        foo(const foo&) = default;
        
        foo(foo&&) = default;
        
        explicit
        foo(const arena& allocator)
          : l(allocator),
            v(allocator),
            s(allocator),
            m(allocator),
            st(allocator),
            d("foo", allocator),
            l1(allocator),
            v1(allocator),
            s1(allocator),
            m1(allocator),
            st1(allocator),
            na(allocator)
        {
        }
        
        
        // Compiler generated operator= OK
        foo& operator=(const foo&) = default;
        foo& operator=(foo&&) = default;

        bool operator==(const foo& other) const
        {
            return true
                && (l == other.l)
                && (v == other.v)
                && (s == other.s)
                && (m == other.m)
                && (st == other.st)
                && (d == other.d)
                && (l1 == other.l1)
                && (v1 == other.v1)
                && (s1 == other.s1)
                && (m1 == other.m1)
                && (st1 == other.st1)
                && (na == other.na);
        }

        bool operator!=(const foo& other) const
        {
            return !(*this == other);
        }

        void swap(foo& other)
        {
            using std::swap;
            swap(l, other.l);
            swap(v, other.v);
            swap(s, other.s);
            swap(m, other.m);
            swap(st, other.st);
            swap(d, other.d);
            swap(l1, other.l1);
            swap(v1, other.v1);
            swap(s1, other.s1);
            swap(m1, other.m1);
            swap(st1, other.st1);
            swap(na, other.na);
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

    
    struct withFoo
    {
        using allocator_type = arena;

        ::test::TheFoo f;
        ::test::foo f1;
        
        template <int = 0> // Workaround to avoid compilation if not used
        withFoo()
        {
        }

        
        // Compiler generated copy ctor OK
        withFoo(const withFoo&) = default;
        
        withFoo(withFoo&&) = default;
        
        explicit
        withFoo(const arena& allocator)
          : f(allocator),
            f1(allocator)
        {
        }
        
        
        // Compiler generated operator= OK
        withFoo& operator=(const withFoo&) = default;
        withFoo& operator=(withFoo&&) = default;

        bool operator==(const withFoo& other) const
        {
            return true
                && (f == other.f)
                && (f1 == other.f1);
        }

        bool operator!=(const withFoo& other) const
        {
            return !(*this == other);
        }

        void swap(withFoo& other)
        {
            using std::swap;
            swap(f, other.f);
            swap(f1, other.f1);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::test::withFoo& left, ::test::withFoo& right)
    {
        left.swap(right);
    }
} // namespace test
