// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/config.h>

#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#error This source file should not be compiled for BOND_LIB_TYPE_HEADER
#endif

#include <bond/core/bond.h>
#include <bond/core/detail/value_extern.h>


namespace bond
{

BOND_DETAIL_INSTANTIATE(BOND_DETAIL_ValueVoid_Apply, BOND_DETAIL_BUILTIN_READERS_WRITERS)

} // namespace bond
