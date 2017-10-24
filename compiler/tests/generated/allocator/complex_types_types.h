
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0700
#error This file was generated by a newer version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0a10
#error This file was generated by an older version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>
#include <bond/core/nullable.h>
#include <bond/core/bonded.h>
#include <bond/core/blob.h>


namespace tests
{
    
    struct Foo
    {
        
        Foo()
        {
        }

        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated copy ctor OK
        Foo(const Foo&) = default;
#endif
        
#if !defined(BOND_NO_CXX11_DEFAULTED_MOVE_CTOR)
        Foo(Foo&&) = default;
#elif !defined(BOND_NO_CXX11_RVALUE_REFERENCES)
        Foo(Foo&&)
        {
        }
#endif
        
        explicit
        Foo(const arena&)
        {
        }
        
        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated operator= OK
        Foo& operator=(const Foo&) = default;
#endif

        bool operator==(const Foo&) const
        {
            return true;
        }

        bool operator!=(const Foo& other) const
        {
            return !(*this == other);
        }

        void swap(Foo&)
        {
            using std::swap;
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::tests::Foo& left, ::tests::Foo& right)
    {
        left.swap(right);
    }

    struct Bar;

    
    struct ComplexTypes
    {
        std::list<int8_t, typename std::allocator_traits<arena>::template rebind_alloc<int8_t> > li8;
        std::set<bool, std::less<bool>, typename std::allocator_traits<arena>::template rebind_alloc<bool> > sb;
        std::vector< ::bond::blob, typename std::allocator_traits<arena>::template rebind_alloc< ::bond::blob> > vb;
        ::bond::nullable<float> nf;
        std::map<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::basic_string<wchar_t, std::char_traits<wchar_t>, typename std::allocator_traits<arena>::template rebind_alloc<wchar_t> >, std::less<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > >, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >, std::basic_string<wchar_t, std::char_traits<wchar_t>, typename std::allocator_traits<arena>::template rebind_alloc<wchar_t> > > > > msws;
        ::bond::bonded< ::tests::Foo> bfoo;
        std::map<double, std::list<std::vector< ::bond::nullable< ::bond::bonded< ::tests::Bar> >, typename std::allocator_traits<arena>::template rebind_alloc< ::bond::nullable< ::bond::bonded< ::tests::Bar> > > >, typename std::allocator_traits<arena>::template rebind_alloc<std::vector< ::bond::nullable< ::bond::bonded< ::tests::Bar> >, typename std::allocator_traits<arena>::template rebind_alloc< ::bond::nullable< ::bond::bonded< ::tests::Bar> > > > > >, std::less<double>, typename std::allocator_traits<arena>::template rebind_alloc<std::pair<const double, std::list<std::vector< ::bond::nullable< ::bond::bonded< ::tests::Bar> >, typename std::allocator_traits<arena>::template rebind_alloc< ::bond::nullable< ::bond::bonded< ::tests::Bar> > > >, typename std::allocator_traits<arena>::template rebind_alloc<std::vector< ::bond::nullable< ::bond::bonded< ::tests::Bar> >, typename std::allocator_traits<arena>::template rebind_alloc< ::bond::nullable< ::bond::bonded< ::tests::Bar> > > > > > > > > m;
        
        struct _bond_vc12_ctor_workaround_ {};
        template <int = 0> // Workaround to avoid compilation if not used
        ComplexTypes(_bond_vc12_ctor_workaround_ = {})
        {
        }

        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated copy ctor OK
        ComplexTypes(const ComplexTypes&) = default;
#endif
        
#if !defined(BOND_NO_CXX11_DEFAULTED_MOVE_CTOR)
        ComplexTypes(ComplexTypes&&) = default;
#elif !defined(BOND_NO_CXX11_RVALUE_REFERENCES)
        ComplexTypes(ComplexTypes&& other)
          : li8(std::move(other.li8)),
            sb(std::move(other.sb)),
            vb(std::move(other.vb)),
            nf(std::move(other.nf)),
            msws(std::move(other.msws)),
            bfoo(std::move(other.bfoo)),
            m(std::move(other.m))
        {
        }
#endif
        
        explicit
        ComplexTypes(const arena& allocator)
          : li8(allocator),
            sb(std::less<bool>(), allocator),
            vb(allocator),
            nf(),
            msws(std::less<std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >>(), allocator),
            m(std::less<double>(), allocator)
        {
        }
        
        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated operator= OK
        ComplexTypes& operator=(const ComplexTypes&) = default;
#endif

        bool operator==(const ComplexTypes& other) const
        {
            return true
                && (li8 == other.li8)
                && (sb == other.sb)
                && (vb == other.vb)
                && (nf == other.nf)
                && (msws == other.msws)
                && (bfoo == other.bfoo)
                && (m == other.m);
        }

        bool operator!=(const ComplexTypes& other) const
        {
            return !(*this == other);
        }

        void swap(ComplexTypes& other)
        {
            using std::swap;
            swap(li8, other.li8);
            swap(sb, other.sb);
            swap(vb, other.vb);
            swap(nf, other.nf);
            swap(msws, other.msws);
            swap(bfoo, other.bfoo);
            swap(m, other.m);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::tests::ComplexTypes& left, ::tests::ComplexTypes& right)
    {
        left.swap(right);
    }
} // namespace tests

#if !defined(BOND_NO_CXX11_ALLOCATOR)
namespace std
{
    template <typename _Alloc>
    struct uses_allocator< ::tests::Foo, _Alloc>
        : is_convertible<_Alloc, arena>
    {};

    template <typename _Alloc>
    struct uses_allocator< ::tests::ComplexTypes, _Alloc>
        : is_convertible<_Alloc, arena>
    {};
}
#endif

