// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/config.hpp>

#if _MSC_VER < 1900
#if defined(BOOST_NO_CXX11_DEFAULTED_FUNCTIONS)
#define BOND_NO_CXX11_DEFAULTED_FUNCTIONS
#endif
#endif

#if defined(BOOST_NO_CXX11_AUTO_DECLARATIONS) || defined(BOOST_NO_AUTO_DECLARATIONS)
#define BOND_NO_CXX11_AUTO_DECLARATIONS
#endif

#if defined(BOOST_NO_CXX11_AUTO_MULTIDECLARATIONS) || defined(BOOST_NO_AUTO_MULTIDECLARATIONS)
#define BOND_NO_CXX11_AUTO_MULTIDECLARATIONS
#endif

#if defined(BOOST_NO_CXX11_LAMBDAS) || defined(BOOST_NO_LAMBDAS)
#define BOND_NO_CXX11_LAMBDAS
#endif

#if defined(BOOST_NO_CXX11_NOEXCEPT) || defined(BOOST_NO_NOEXCEPT)
#define BOND_NO_CXX11_NOEXCEPT
#endif

#if defined(BOOST_NO_CXX11_NULLPTR) || defined(BOOST_NO_NULLPTR)
#define BOND_NO_CXX11_NULLPTR
#endif

#if defined(BOOST_NO_CXX11_RVALUE_REFERENCES) || defined(BOOST_NO_RVALUE_REFERENCES)
#define BOND_NO_CXX11_RVALUE_REFERENCES
#endif

#if defined(BOOST_NO_CXX11_SCOPED_ENUMS) || defined(BOOST_NO_SCOPED_ENUMS)
#define BOND_NO_CXX11_SCOPED_ENUMS
#endif

#if defined(BOOST_NO_CXX11_VARIADIC_TEMPLATES) || defined(BOOST_NO_VARIADIC_TEMPLATES)
#define BOND_NO_CXX11_VARIADIC_TEMPLATES
#endif

#if defined(BOOST_NO_CXX11_HDR_CODECVT) || defined(BOOST_NO_0X_HDR_CODECVT)
#define BOND_NO_CXX11_HDR_CODECVT
#endif

#if defined(BOOST_NO_CXX11_HDR_TYPE_TRAITS) && (_CPPLIB_VER < 520) && !defined(__GXX_EXPERIMENTAL_CXX0X__)
#define BOND_NO_CXX11_HDR_TYPE_TRAITS
#endif

// std::once seems problematic on Linux (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60662)
// For now we use std::call_once only on MSVC and boost::call_once on GCC/Clang.
#if defined(BOOST_NO_CXX11_HDR_MUTEX) || !defined(_MSC_VER)
#define BOND_NO_CX11_HDR_MUTEX
#endif

#if defined(BOOST_NO_CXX11_ALLOCATOR)
#define BOND_NO_CXX11_ALLOCATOR
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

#define BOND_NOEXCEPT BOOST_NOEXCEPT_OR_NOTHROW

