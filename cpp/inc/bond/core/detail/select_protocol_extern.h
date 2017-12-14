// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "extern_macro.h"


namespace bond
{
namespace detail
{

#define BOND_DETAIL_INSTANTIATE_NextProtocol(Writer) \
    template std::pair<ProtocolType, bool> NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Serializer<Writer>&);

#define BOND_DETAIL_EXTERN_NextProtocol(Writer) \
    extern BOND_DETAIL_INSTANTIATE_NextProtocol(Writer)

BOND_DETAIL_PRECOMPILE_WRITERS(BOND_DETAIL_EXTERN_NextProtocol)


#define BOND_DETAIL_INSTANTIATE_NextProtocol_Null() \
    template std::pair<ProtocolType, bool> NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Null&);

#define BOND_DETAIL_EXTERN_NextProtocol_Null() \
    extern BOND_DETAIL_INSTANTIATE_NextProtocol_Null()

BOND_DETAIL_EXTERN_NextProtocol_Null()


#define BOND_DETAIL_INSTANTIATE_NextProtocol_Select(Writer) \
    template bool NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Serializer<Writer>&, uint16_t);

#define BOND_DETAIL_EXTERN_NextProtocol_Select(Writer) \
    extern BOND_DETAIL_INSTANTIATE_NextProtocol_Select(Writer)

BOND_DETAIL_PRECOMPILE_WRITERS(BOND_DETAIL_EXTERN_NextProtocol_Select)


#define BOND_DETAIL_INSTANTIATE_NextProtocol_Select_Null() \
    template bool NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Null&, uint16_t);

#define BOND_DETAIL_EXTERN_NextProtocol_Select_Null() \
    extern BOND_DETAIL_INSTANTIATE_NextProtocol_Select_Null()

BOND_DETAIL_EXTERN_NextProtocol_Select_Null()


} // namespace detail
} // namespace bond
