// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include "extern_macro.h"


namespace bond
{
namespace detail 
{

#define BOND_DETAIL_BasicTypeField(Reader, Writer) \
    bool BasicTypeField(uint16_t, const Metadata&, BondDataType, const Serializer<Writer>&, Reader&);

BOND_DETAIL_EXTERN(BOND_DETAIL_BasicTypeField, BOND_DETAIL_BUILTIN_READERS_WRITERS)


#define BOND_DETAIL_BasicTypeField_Null(Reader) \
    bool BasicTypeField(uint16_t, const Metadata&, BondDataType, const Null&, Reader&);

BOND_DETAIL_EXTERN(BOND_DETAIL_BasicTypeField_Null, BOND_DETAIL_BUILTIN_READERS)


#define BOND_DETAIL_BasicTypeContainer(Reader, Writer) \
    void BasicTypeContainer<BuiltInProtocols>(const Serializer<Writer>&, BondDataType, Reader&, uint32_t);

BOND_DETAIL_EXTERN(BOND_DETAIL_BasicTypeContainer, BOND_DETAIL_BUILTIN_READERS_WRITERS)


#define BOND_DETAIL_MapByKey(Reader, Writer, T) \
    void MapByKey<BuiltInProtocols>( \
        const Serializer<Writer>&, BondDataType, const value<T, Reader&>&, Reader&, uint32_t);

BOND_DETAIL_EXTERN(BOND_DETAIL_MapByKey, BOND_DETAIL_BUILTIN_READERS_WRITERS_BASIC_TYPES)


#define BOND_DETAIL_MapByElement(Reader, Writer) \
    void MapByElement<BuiltInProtocols>(const Serializer<Writer>&, BondDataType, BondDataType, Reader&, uint32_t);

BOND_DETAIL_EXTERN(BOND_DETAIL_MapByElement, BOND_DETAIL_BUILTIN_READERS_WRITERS)


} // namespace detail
} // namespace bond
