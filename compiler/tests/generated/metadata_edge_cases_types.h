
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
    
    struct Foo
    {
        int32_t id;
        int32_t metadata;
        int32_t _id;
        int32_t x;
        int32_t _x;
        
        Foo()
          : id(),
            metadata(),
            _id(),
            x(),
            _x()
        {
        }

        
        // Compiler generated copy ctor OK
        Foo(const Foo&) = default;
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        Foo(Foo&& other)
          : id(std::move(other.id)),
            metadata(std::move(other.metadata)),
            _id(std::move(other._id)),
            x(std::move(other.x)),
            _x(std::move(other._x))
        {
        }
#else
        Foo(Foo&&) = default;
#endif
        
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        Foo& operator=(Foo other)
        {
            other.swap(*this);
            return *this;
        }
#else
        // Compiler generated operator= OK
        Foo& operator=(const Foo&) = default;
        Foo& operator=(Foo&&) = default;
#endif

        bool operator==(const Foo& other) const
        {
            return true
                && (id == other.id)
                && (metadata == other.metadata)
                && (_id == other._id)
                && (x == other.x)
                && (_x == other._x);
        }

        bool operator!=(const Foo& other) const
        {
            return !(*this == other);
        }

        void swap(Foo& other)
        {
            using std::swap;
            swap(id, other.id);
            swap(metadata, other.metadata);
            swap(_id, other._id);
            swap(x, other.x);
            swap(_x, other._x);
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
} // namespace tests
