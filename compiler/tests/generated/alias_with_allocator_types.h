
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x302
#error This file was generated by a newer version of Bond compiler
#error and is incompatible with your version Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0410
#error This file was generated by an older version of Bond compiler
#error and is incompatible with your version Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>



namespace test
{
    
    struct foo
    {
        std::list<bool, typename arena::rebind<bool>::other> l;
        std::vector<bool, typename arena::rebind<bool>::other> v;
        std::set<bool, std::less<bool>, typename arena::rebind<bool>::other> s;
        std::map<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>, bool, std::less<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other> >, typename arena::rebind<std::pair<const std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>, bool> >::other> m;
        std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other> st;
        std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other> d;
        bond::maybe<std::list<bool, typename arena::rebind<bool>::other> > l1;
        bond::maybe<std::vector<bool, typename arena::rebind<bool>::other> > v1;
        bond::maybe<std::set<bool, std::less<bool>, typename arena::rebind<bool>::other> > s1;
        bond::maybe<std::map<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>, bool, std::less<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other> >, typename arena::rebind<std::pair<const std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>, bool> >::other> > m1;
        bond::maybe<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other> > st1;
        
        foo()
          : d("foo")
        {
        }

        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated copy ctor OK
        foo(const foo& other) = default;
#endif
        
#if !defined(BOND_NO_CXX11_DEFAULTED_MOVE_CTOR)
        foo(foo&& other) = default;
#elif !defined(BOND_NO_CXX11_RVALUE_REFERENCES)
        foo(foo&& other)
          : l(std::move(other.l)),
            v(std::move(other.v)),
            s(std::move(other.s)),
            m(std::move(other.m)),
            st(std::move(other.st)),
            d(std::move(other.d)),
            l1(std::move(other.l1)),
            v1(std::move(other.v1)),
            s1(std::move(other.s1)),
            m1(std::move(other.m1)),
            st1(std::move(other.st1))
        {
        }
#endif
        
        explicit
        foo(const arena& allocator)
          : l(allocator),
            v(allocator),
            s(std::less<bool>(), allocator),
            m(std::less<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>>(), allocator),
            st(allocator),
            d("foo", allocator),
            l1(allocator),
            v1(allocator),
            s1(std::less<bool>(), allocator),
            m1(std::less<std::basic_string<char, std::char_traits<char>, typename arena::rebind<char>::other>>(), allocator),
            st1(allocator)
        {
        }
        
        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated operator= OK
        foo& operator=(const foo& other) = default;
#endif

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
                && (st1 == other.st1);
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
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(foo& left, foo& right)
    {
        left.swap(right);
    }

    
    struct withFoo
    {
        ::test::foo f;
        ::test::foo f1;
        
        withFoo()
        {
        }

        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated copy ctor OK
        withFoo(const withFoo& other) = default;
#endif
        
#if !defined(BOND_NO_CXX11_DEFAULTED_MOVE_CTOR)
        withFoo(withFoo&& other) = default;
#elif !defined(BOND_NO_CXX11_RVALUE_REFERENCES)
        withFoo(withFoo&& other)
          : f(std::move(other.f)),
            f1(std::move(other.f1))
        {
        }
#endif
        
        explicit
        withFoo(const arena& allocator)
          : f(allocator),
            f1(allocator)
        {
        }
        
        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated operator= OK
        withFoo& operator=(const withFoo& other) = default;
#endif

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

    inline void swap(withFoo& left, withFoo& right)
    {
        left.swap(right);
    }
} // namespace test

#if !defined(BOND_NO_CXX11_ALLOCATOR)
namespace std
{
    template <typename _Alloc>
    struct uses_allocator< ::test::foo, _Alloc>
        : is_convertible<_Alloc, arena>
    {};

    template <typename _Alloc>
    struct uses_allocator< ::test::withFoo, _Alloc>
        : is_convertible<_Alloc, arena>
    {};
}
#endif

