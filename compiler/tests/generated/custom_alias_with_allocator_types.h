
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0700
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
        my::list<bool, arena> l;
        my::vector<bool, arena> v;
        my::set<bool, arena> s;
        my::map<my::string<arena>, bool, arena> m;
        my::string<arena> st;
        my::string<arena> d;
        ::bond::maybe<my::list<bool, arena> > l1;
        ::bond::maybe<my::vector<bool, arena> > v1;
        ::bond::maybe<my::set<bool, arena> > s1;
        ::bond::maybe<my::map<my::string<arena>, bool, arena> > m1;
        ::bond::maybe<my::string<arena> > st1;
        my::set<my::list<my::map<int32_t, my::string<arena>, arena>, arena>, arena> na;
        
        struct _bond_vc12_ctor_workaround_ {};
        template <int = 0> // Workaround to avoid compilation if not used
        foo(_bond_vc12_ctor_workaround_ = {})
          : l(),
            v(),
            s(),
            m(),
            st(),
            d("foo"),
            na()
        {
        }

        
        // Compiler generated copy ctor OK
        foo(const foo&) = default;
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
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
            st1(std::move(other.st1)),
            na(std::move(other.na))
        {
        }
#else
        foo(foo&&) = default;
#endif
        
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
} // namespace test

namespace std
{
    template <typename _Alloc>
    struct uses_allocator< ::test::foo, _Alloc>
        : is_convertible<_Alloc, arena>
    {};
}

