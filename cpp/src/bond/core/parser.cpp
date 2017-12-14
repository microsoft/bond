// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/config.h>

#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#error This source file should not be compiled for BOND_LIB_TYPE_HEADER
#endif

#include <bond/core/bond.h>
#include <bond/core/detail/parser_extern.h>


namespace bond
{

BOND_DETAIL_INSTANTIATE(
    BOND_DETAIL_StaticParser_ReadFields,
    (BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY)
    BOND_DETAIL_BUILTIN_WRITERS)

BOND_DETAIL_INSTANTIATE(
    BOND_DETAIL_StaticParser_ReadFields_Null,
    (BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY))

BOND_DETAIL_INSTANTIATE(
    BOND_DETAIL_DynamicParser_ReadFields,
    (BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOND_DETAIL_BUILTIN_READER_FAST_BINARY)
    BOND_DETAIL_BUILTIN_WRITERS)

BOND_DETAIL_INSTANTIATE(
    BOND_DETAIL_DynamicParser_ReadFields_Null,
    (BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOND_DETAIL_BUILTIN_READER_FAST_BINARY))

} // namespace bond
