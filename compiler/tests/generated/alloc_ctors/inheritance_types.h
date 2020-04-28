
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0800
#error This file was generated by a newer version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0c01
#error This file was generated by an older version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>



namespace tests
{
    
    struct Base
    {
        using allocator_type = arena;

        int32_t x;
        
        Base()
          : x()
        {
        }

        
        // Compiler generated copy ctor OK
        Base(const Base&) = default;

        Base(const Base& other, const arena&)
          : x(other.x)
        {
        }
        
        Base(Base&&) = default;

        Base(Base&& other, const arena&)
          : x(std::move(other.x))
        {
        }
        
        explicit
        Base(const arena&)
          : x()
        {
        }
        
        
        // Compiler generated operator= OK
        Base& operator=(const Base&) = default;
        Base& operator=(Base&&) = default;

        bool operator==(const Base& other) const
        {
            return true
                && (x == other.x);
        }

        bool operator!=(const Base& other) const
        {
            return !(*this == other);
        }

        void swap(Base& other)
        {
            using std::swap;
            swap(x, other.x);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::tests::Base& left, ::tests::Base& right)
    {
        left.swap(right);
    }

    
    struct Foo
      : ::tests::Base
    {
        using allocator_type = arena;

        int32_t x;
        
        template <int = 0> // Workaround to avoid compilation if not used
        Foo()
          : x()
        {
        }

        
        // Compiler generated copy ctor OK
        Foo(const Foo&) = default;

        Foo(const Foo& other, const arena& allocator)
          : ::tests::Base(other, allocator),
            x(other.x)
        {
        }
        
        Foo(Foo&&) = default;

        Foo(Foo&& other, const arena& allocator)
          : ::tests::Base(std::move(other), allocator),
            x(std::move(other.x))
        {
        }
        
        explicit
        Foo(const arena& allocator)
          : ::tests::Base(allocator),
            x()
        {
        }
        
        
        // Compiler generated operator= OK
        Foo& operator=(const Foo&) = default;
        Foo& operator=(Foo&&) = default;

        bool operator==(const Foo& other) const
        {
            return true
                && (static_cast<const ::tests::Base&>(*this) == static_cast<const ::tests::Base&>( other))
                && (x == other.x);
        }

        bool operator!=(const Foo& other) const
        {
            return !(*this == other);
        }

        void swap(Foo& other)
        {
            using std::swap;
            ::tests::Base::swap( other);
            swap(x, other.x);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*name, const char*qual_name)
        {
            ::tests::Base::InitMetadata(name, qual_name);
        }
    };

    inline void swap(::tests::Foo& left, ::tests::Foo& right)
    {
        left.swap(right);
    }
} // namespace tests
