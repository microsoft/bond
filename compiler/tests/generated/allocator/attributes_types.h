
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



namespace tests
{
    
    namespace _bond_enumerators
    {
    namespace Enum
    {
        enum Enum
        {
            Value1
        };
        
        inline BOND_CONSTEXPR const char* GetTypeName(enum Enum)
        {
            return "Enum";
        }

        inline BOND_CONSTEXPR const char* GetTypeName(enum Enum, const ::bond::qualified_name_tag&)
        {
            return "tests.Enum";
        }

        template <typename Map = std::map<enum Enum, std::string> >
        inline const Map& GetValueToNameMap(enum Enum)
        {
            static const Map _value_to_name_Enum
                {
                    { Value1, "Value1" }
                };
            return _value_to_name_Enum;
        }

        template <typename Map = std::map<std::string, enum Enum> >
        inline const Map& GetNameToValueMap(enum Enum)
        {
            static const Map _name_to_value_Enum
                {
                    { "Value1", Value1 }
                };
            return _name_to_value_Enum;
        }

        const std::string& ToString(enum Enum value);

        void FromString(const std::string& name, enum Enum& value);

        bool ToEnum(enum Enum& value, const std::string& name);

        bool FromEnum(std::string& name, enum Enum value);

    } // namespace Enum
    } // namespace _bond_enumerators

    using namespace _bond_enumerators::Enum;
    

    
    struct Foo
    {
        std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> > f;
        
        struct _bond_vc12_ctor_workaround_ {};
        template <int = 0> // Workaround to avoid compilation if not used
        Foo(_bond_vc12_ctor_workaround_ = {})
        {
        }

        
        // Compiler generated copy ctor OK
        Foo(const Foo&) = default;
        
#if defined(_MSC_VER) && (_MSC_VER < 1900)  // Versions of MSVC prior to 1900 do not support = default for move ctors
        Foo(Foo&& other)
          : f(std::move(other.f))
        {
        }
#else
        Foo(Foo&&) = default;
#endif
        
        explicit
        Foo(const arena& allocator)
          : f(allocator)
        {
        }
        
        
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
                && (f == other.f);
        }

        bool operator!=(const Foo& other) const
        {
            return !(*this == other);
        }

        void swap(Foo& other)
        {
            using std::swap;
            swap(f, other.f);
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

namespace std
{
    template <typename _Alloc>
    struct uses_allocator< ::tests::Foo, _Alloc>
        : is_convertible<_Alloc, arena>
    {};
}

