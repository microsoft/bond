
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0800
#error This file was generated by a newer version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0b02
#error This file was generated by an older version of the Bond compiler and is incompatible with your version of the Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>


#include "dir1/dir2/empty_types.h"

namespace import_test
{
    
    struct HasEmpty
    {
        using allocator_type = arena;

        ::empty::Empty e;
        
        struct _bond_vc12_ctor_workaround_ {};
        template <int = 0> // Workaround to avoid compilation if not used
        HasEmpty(_bond_vc12_ctor_workaround_ = {})
        {
        }

        
        // Compiler generated copy ctor OK
        HasEmpty(const HasEmpty&) = default;
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        HasEmpty(HasEmpty&& other)
          : e(std::move(other.e))
        {
        }
#else
        HasEmpty(HasEmpty&&) = default;
#endif
        
        explicit
        HasEmpty(const arena& allocator)
          : e(allocator)
        {
        }
        
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        HasEmpty& operator=(HasEmpty other)
        {
            other.swap(*this);
            return *this;
        }
#else
        // Compiler generated operator= OK
        HasEmpty& operator=(const HasEmpty&) = default;
        HasEmpty& operator=(HasEmpty&&) = default;
#endif

        bool operator==(const HasEmpty& other) const
        {
            return true
                && (e == other.e);
        }

        bool operator!=(const HasEmpty& other) const
        {
            return !(*this == other);
        }

        void swap(HasEmpty& other)
        {
            using std::swap;
            swap(e, other.e);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::import_test::HasEmpty& left, ::import_test::HasEmpty& right)
    {
        left.swap(right);
    }
} // namespace import_test
