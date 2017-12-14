// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/config.h>

#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#error This source file should not be compiled for BOND_LIB_TYPE_HEADER
#endif

#include <bond/core/bond.h>
#include <bond/core/detail/typeid_value_extern.h>


namespace bond
{
namespace detail
{
    BOND_DETAIL_INSTANTIATE(BOND_DETAIL_BasicTypeField, BOND_DETAIL_BUILTIN_READERS_WRITERS)

    BOND_DETAIL_INSTANTIATE(BOND_DETAIL_BasicTypeField_Null, BOND_DETAIL_BUILTIN_READERS)

    BOND_DETAIL_INSTANTIATE(BOND_DETAIL_BasicTypeContainer, BOND_DETAIL_BUILTIN_READERS_WRITERS)

    BOND_DETAIL_INSTANTIATE(BOND_DETAIL_MapByKey, BOND_DETAIL_BUILTIN_READERS_WRITERS_BASIC_TYPES)

    BOND_DETAIL_INSTANTIATE(BOND_DETAIL_MapByElement, BOND_DETAIL_BUILTIN_READERS_WRITERS)

} // namespace detail
} // namespace bond
