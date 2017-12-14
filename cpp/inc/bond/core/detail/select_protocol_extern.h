// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include "extern_macro.h"


namespace bond
{
namespace detail
{

#define BOND_DETAIL_NextProtocol(Writer) \
    std::pair<ProtocolType, bool> NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Serializer<Writer>&);

BOND_DETAIL_EXTERN(BOND_DETAIL_NextProtocol, BOND_DETAIL_BUILTIN_WRITERS)


#define BOND_DETAIL_NextProtocol_Null() \
    std::pair<ProtocolType, bool> NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Null&);

BOND_DETAIL_PREFIX_EXTERN(BOND_DETAIL_NextProtocol_Null)()


#define BOND_DETAIL_NextProtocol_Select(Writer) \
    bool NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Serializer<Writer>&, uint16_t);

BOND_DETAIL_EXTERN(BOND_DETAIL_NextProtocol_Select, BOND_DETAIL_BUILTIN_WRITERS)


#define BOND_DETAIL_NextProtocol_Select_Null() \
    bool NextProtocol<BuiltInProtocols>( \
        const RuntimeSchema&, InputBuffer&, const Null&, uint16_t);

BOND_DETAIL_PREFIX_EXTERN(BOND_DETAIL_NextProtocol_Select_Null)()


} // namespace detail
} // namespace bond
