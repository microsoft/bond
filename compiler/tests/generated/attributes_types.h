
#pragma once

#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x0422
#error This file was generated by a newer version of Bond compiler
#error and is incompatible with your version Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x0500
#error This file was generated by an older version of Bond compiler
#error and is incompatible with your version Bond library.
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
        
        extern const std::map<enum Enum, std::string> _value_to_name_Enum;
        extern const std::map<std::string, enum Enum> _name_to_value_Enum;

        inline
        const char* GetTypeName(enum Enum)
        {
            return "Enum";
        }

        inline
        const char* GetTypeName(enum Enum, const bond::qualified_name_tag&)
        {
            return "tests.Enum";
        }

        inline
        const std::map<enum Enum, std::string>& GetValueToNameMap(enum Enum)
        {
            return _value_to_name_Enum;
        }

        inline
        const std::map<std::string, enum Enum>& GetNameToValueMap(enum Enum)
        {
            return _name_to_value_Enum;
        }

        const std::string& ToString(enum Enum value);

        void FromString(const std::string& name, enum Enum& value);

        inline
        bool ToEnum(enum Enum& value, const std::string& name)
        {
            std::map<std::string, enum Enum>::const_iterator it =
                _name_to_value_Enum.find(name);

            if (_name_to_value_Enum.end() == it)
                return false;

            value = it->second;

            return true;
        }
    } // namespace Enum
    } // namespace _bond_enumerators

    using namespace _bond_enumerators::Enum;
    

    
    struct Foo
    {
        std::string f;
        
        Foo()
        {
        }

        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated copy ctor OK
        Foo(const Foo& other) = default;
#endif
        
#if !defined(BOND_NO_CXX11_DEFAULTED_MOVE_CTOR)
        Foo(Foo&& other) = default;
#elif !defined(BOND_NO_CXX11_RVALUE_REFERENCES)
        Foo(Foo&& other)
          : f(std::move(other.f))
        {
        }
#endif
        
        
#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
        // Compiler generated operator= OK
        Foo& operator=(const Foo& other) = default;
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

