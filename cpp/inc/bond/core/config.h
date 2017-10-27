// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/config.hpp>

#if defined(BOOST_NO_CXX11_NOEXCEPT) || defined(BOOST_NO_NOEXCEPT)
#define BOND_NO_CXX11_NOEXCEPT
#endif

#if defined(BOOST_NO_CXX11_DEFAULTED_FUNCTIONS) \
    || defined(BOOST_NO_CXX11_RVALUE_REFERENCES) \
    || defined(BOOST_NO_RVALUE_REFERENCES)
// We need support for both defaulted functions and rvalue references to use
// default move ctors
#define BOND_NO_CXX11_DEFAULTED_MOVE_CTOR
#else
    #if defined(_MSC_VER) && (_MSC_VER < 1900)
    // Versions of MSVC prior to 1900 do not support = default for move ctors
    #define BOND_NO_CXX11_DEFAULTED_MOVE_CTOR
    #endif
#endif

#if defined(BOOST_NO_CXX11_HDR_TYPE_TRAITS) && (_CPPLIB_VER < 520) && !defined(__GXX_EXPERIMENTAL_CXX0X__)
#define BOND_NO_CXX11_HDR_TYPE_TRAITS
#endif

// std::once seems problematic on Linux (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60662)
// For now we use std::call_once only on MSVC and boost::call_once on GCC/Clang.
#if defined(BOOST_NO_CXX11_HDR_MUTEX) || !defined(_MSC_VER)
#define BOND_NO_CX11_HDR_MUTEX
#endif

#if defined(BOOST_NO_CXX11_ALLOCATOR) || defined(_LIBCPP_HAS_NO_TEMPLATE_ALIASES)
#define BOND_NO_CXX11_ALLOCATOR
#endif

#if defined(BOOST_NO_CXX14_RETURN_TYPE_DEDUCTION)
#define BOND_NO_CXX14_RETURN_TYPE_DEDUCTION
#endif

#if defined(BOOST_NO_CXX14_GENERIC_LAMBDAS)
#define BOND_NO_CXX14_GENERIC_LAMBDAS
#endif

#ifdef _MSC_VER
#define BOND_CALL       __cdecl
#define BOND_NO_INLINE  __declspec(noinline)
#elif defined(__GNUC__) && defined(__i386) && !defined(__INTEL_COMPILER)
#define BOND_CALL       __attribute__((cdecl))
#define BOND_NO_INLINE  __attribute__((noinline))
#else
#define BOND_CALL
#define BOND_NO_INLINE  __attribute__((noinline))
#endif

#define BOND_NOEXCEPT           BOOST_NOEXCEPT_OR_NOTHROW
#define BOND_NOEXCEPT_IF        BOOST_NOEXCEPT_IF
#define BOND_NORETURN           BOOST_NORETURN
#define BOND_CONSTEXPR          BOOST_CONSTEXPR
#define BOND_CONSTEXPR_OR_CONST BOOST_CONSTEXPR_OR_CONST
#define BOND_STATIC_CONSTEXPR   BOOST_STATIC_CONSTEXPR
