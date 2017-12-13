// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <bond/core/bond.h>

#if BOND_LIB_TYPE == BOND_LIB_TYPE_HEADER
#error This source file should not be compiled for BOND_LIB_TYPE_HEADER
#endif

#include <bond/core/detail/typeid_value_extern.h>


namespace bond
{
namespace detail
{
    BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_INSTANTIATE_BasicTypeField)

    BOND_DETAIL_PRECOMPILE_READERS(BOND_DETAIL_INSTANTIATE_BasicTypeField_Null)

    BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_INSTANTIATE_BasicTypeContainer)

    BOND_DETAIL_PRECOMPILE_READERS_WRITERS_BASIC_TYPES(BOND_DETAIL_INSTANTIATE_MapByKey)

    BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_INSTANTIATE_MapByElement)

} // namespace detail
} // namespace bond
