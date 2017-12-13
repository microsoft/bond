// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/bond.h>

#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#error This source file should not be compiled for BOND_LIB_TYPE_HEADER
#endif

#include <bond/core/detail/parser_extern.h>


namespace bond
{

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields,
    ((SimpleBinaryReader<InputBuffer>))
    (BOND_DETAIL_BUILTIN_WRITERS))

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields_Null,
    ((SimpleBinaryReader<InputBuffer>)))

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields,
    ((CompactBinaryReader<InputBuffer>)(FastBinaryReader<InputBuffer>))
    (BOND_DETAIL_BUILTIN_WRITERS))

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields_Null,
    ((CompactBinaryReader<InputBuffer>)(FastBinaryReader<InputBuffer>)))

} // namespace bond
