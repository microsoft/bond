// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#if !defined(BOND_NO_CX11_HDR_MUTEX)

// Use std::call_once when building with a sane compiler.
#include <mutex>

namespace bond
{
namespace detail
{

using std::once_flag;
using std::call_once;

} // namespace detail
} // namespace bond

#else

#include <boost/thread/once.hpp>

namespace bond
{
namespace detail
{

using boost::once_flag;
using boost::call_once;

} // namespace detail
} // namespace bond

#endif
