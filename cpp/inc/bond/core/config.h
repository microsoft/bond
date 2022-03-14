// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/config.hpp>

#if !defined(BOND_COMPACT_BINARY_PROTOCOL) \
 && !defined(BOND_SIMPLE_BINARY_PROTOCOL) \
 && !defined(BOND_FAST_BINARY_PROTOCOL) \
 && !defined(BOND_SIMPLE_JSON_PROTOCOL)

#define BOND_COMPACT_BINARY_PROTOCOL
#define BOND_SIMPLE_BINARY_PROTOCOL
#define BOND_FAST_BINARY_PROTOCOL
// BOND_SIMPLE_JSON_PROTOCOL disabled by default
#endif

// std::once seems problematic on Linux (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60662)
// For now we use std::call_once only on MSVC and boost::call_once on GCC/Clang.
#if defined(BOOST_NO_CXX11_HDR_MUTEX) || !defined(_MSC_VER)
#define BOND_NO_CX11_HDR_MUTEX
#endif

// MSVC 14 (VS 2015) has only a dangerous partial implementation of expression SFINAE.
#if defined(BOOST_NO_SFINAE_EXPR) || (defined(_MSC_VER) && (_MSC_VER < 1910))
#define BOND_NO_SFINAE_EXPR
#endif

// MSVC doesn't set __cplusplus to the C++ standard level unless the extra
// /Zc:__cplusplus switch is passed (to prevent breaking existing code that
// assumes it is always equal to 199711L). We don't want to require users of
// Bond to have to pass this, so we also check _MSVC_LANG, which is always
// set to the C++ standard level since MSVC 2015.
#if (__cplusplus >= 201703L || (defined(_MSVC_LANG) && (_MSVC_LANG >= 201703L)))
    #define BOND_CXX_17
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
#define BOND_NOEXCEPT_EXPR      BOOST_NOEXCEPT_EXPR
#define BOND_CONSTEXPR          BOOST_CONSTEXPR
#define BOND_CONSTEXPR_OR_CONST BOOST_CONSTEXPR_OR_CONST
#define BOND_STATIC_CONSTEXPR   BOOST_STATIC_CONSTEXPR


#define BOND_LIB_TYPE_HEADER    1
#define BOND_LIB_TYPE_STATIC    2
#define BOND_LIB_TYPE_DYNAMIC   3   // Not implemented

#ifndef BOND_LIB_TYPE
#define BOND_LIB_TYPE BOND_LIB_TYPE_HEADER
#elif (BOND_LIB_TYPE != BOND_LIB_TYPE_HEADER) && (BOND_LIB_TYPE != BOND_LIB_TYPE_STATIC)
#error Unsupported library type is defined for BOND_LIB_TYPE
#endif

#ifndef BOND_DETAIL_HEADER_ONLY_INLINE
#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#define BOND_DETAIL_HEADER_ONLY_INLINE inline
#else
#define BOND_DETAIL_HEADER_ONLY_INLINE
#endif
#else
#error BOND_DETAIL_HEADER_ONLY_INLINE is already defined
#endif
