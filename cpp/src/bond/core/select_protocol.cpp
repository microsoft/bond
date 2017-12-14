// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/config.h>

#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#error This source file should not be compiled for BOND_LIB_TYPE_HEADER
#endif

#include <bond/core/bond.h>
#include <bond/core/detail/select_protocol_extern.h>


namespace bond
{
namespace detail
{

BOND_DETAIL_INSTANTIATE(BOND_DETAIL_NextProtocol, BOND_DETAIL_BUILTIN_WRITERS)

BOND_DETAIL_PREFIX_TEMPLATE(BOND_DETAIL_NextProtocol_Null)()

BOND_DETAIL_INSTANTIATE(BOND_DETAIL_NextProtocol_Select, BOND_DETAIL_BUILTIN_WRITERS)

BOND_DETAIL_PREFIX_TEMPLATE(BOND_DETAIL_NextProtocol_Select_Null)()

} // namespace detail
} // namespace bond
