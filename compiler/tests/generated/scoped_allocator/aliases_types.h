
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
#include <scoped_allocator>


namespace tests
{
    
    template <typename T>
    struct Foo
    {
        using allocator_type = arena;

        std::vector<std::vector<T, std::scoped_allocator_adaptor<typename std::allocator_traits<arena>::template rebind_alloc<T> > >, std::scoped_allocator_adaptor<typename std::allocator_traits<arena>::template rebind_alloc<std::vector<T, std::scoped_allocator_adaptor<typename std::allocator_traits<arena>::template rebind_alloc<T> > > > > > aa;
        
        template <int = 0> // Workaround to avoid compilation if not used
        Foo()
        {
        }

        
        // Compiler generated copy ctor OK
        Foo(const Foo&) = default;
        
        Foo(Foo&&) = default;
        
        explicit
        Foo(const arena& allocator)
          : aa(allocator)
        {
        }
        
        
        // Compiler generated operator= OK
        Foo& operator=(const Foo&) = default;
        Foo& operator=(Foo&&) = default;

        bool operator==(const Foo& other) const
        {
            return true
                && (aa == other.aa);
        }

        bool operator!=(const Foo& other) const
        {
            return !(*this == other);
        }

        void swap(Foo& other)
        {
            using std::swap;
            swap(aa, other.aa);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    template <typename T>
    inline void swap(::tests::Foo<T>& left, ::tests::Foo<T>& right)
    {
        left.swap(right);
    }

    
    namespace _bond_enumerators
    {
    namespace EnumToWrap
    {
        enum EnumToWrap
        {
            anEnumValue
        };
        
        inline BOND_CONSTEXPR const char* GetTypeName(enum EnumToWrap)
        {
            return "EnumToWrap";
        }

        inline BOND_CONSTEXPR const char* GetTypeName(enum EnumToWrap, const ::bond::qualified_name_tag&)
        {
            return "tests.EnumToWrap";
        }


        template <typename Map = std::map<enum EnumToWrap, std::string> >
        inline const Map& GetValueToNameMap(enum EnumToWrap, ::bond::detail::mpl::identity<Map> = {})
        {
            static const Map s_valueToNameMap
                {
                    { anEnumValue, "anEnumValue" }
                };
            return s_valueToNameMap;
        }

        template <typename Map = std::map<std::string, enum EnumToWrap> >
        inline const Map& GetNameToValueMap(enum EnumToWrap, ::bond::detail::mpl::identity<Map> = {})
        {
            static const Map s_nameToValueMap
                {
                    { "anEnumValue", anEnumValue }
                };
            return s_nameToValueMap;
        }
        const std::string& ToString(enum EnumToWrap value);

        void FromString(const std::string& name, enum EnumToWrap& value);

        bool ToEnum(enum EnumToWrap& value, const std::string& name);

        bool FromEnum(std::string& name, enum EnumToWrap value);

    } // namespace EnumToWrap
    } // namespace _bond_enumerators

    using namespace _bond_enumerators::EnumToWrap;
    

    
    struct WrappingAnEnum
    {
        using allocator_type = arena;

        ::tests::EnumToWrap aWrappedEnum;
        
        WrappingAnEnum()
          : aWrappedEnum(::tests::_bond_enumerators::EnumToWrap::anEnumValue)
        {
        }

        
        // Compiler generated copy ctor OK
        WrappingAnEnum(const WrappingAnEnum&) = default;
        
        WrappingAnEnum(WrappingAnEnum&&) = default;
        
        explicit
        WrappingAnEnum(const arena&)
          : aWrappedEnum(::tests::_bond_enumerators::EnumToWrap::anEnumValue)
        {
        }
        
        
        // Compiler generated operator= OK
        WrappingAnEnum& operator=(const WrappingAnEnum&) = default;
        WrappingAnEnum& operator=(WrappingAnEnum&&) = default;

        bool operator==(const WrappingAnEnum& other) const
        {
            return true
                && (aWrappedEnum == other.aWrappedEnum);
        }

        bool operator!=(const WrappingAnEnum& other) const
        {
            return !(*this == other);
        }

        void swap(WrappingAnEnum& other)
        {
            using std::swap;
            swap(aWrappedEnum, other.aWrappedEnum);
        }

        struct Schema;

    protected:
        void InitMetadata(const char*, const char*)
        {
        }
    };

    inline void swap(::tests::WrappingAnEnum& left, ::tests::WrappingAnEnum& right)
    {
        left.swap(right);
    }
} // namespace tests
