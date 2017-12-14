// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "extern_macro.h"


namespace bond
{
namespace detail 
{

#define BOND_DETAIL_INSTANTIATE_BasicTypeField(Reader, Writer) \
    template bool BasicTypeField(uint16_t, const Metadata&, BondDataType, const Serializer<Writer>&, Reader&);

#define BOND_DETAIL_EXTERN_BasicTypeField(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_BasicTypeField(Reader, Writer)

BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_EXTERN_BasicTypeField)


#define BOND_DETAIL_INSTANTIATE_BasicTypeField_Null(Reader) \
    template bool BasicTypeField(uint16_t, const Metadata&, BondDataType, const Null&, Reader&);

#define BOND_DETAIL_EXTERN_BasicTypeField_Null(Reader) \
    extern BOND_DETAIL_INSTANTIATE_BasicTypeField_Null(Reader)

BOND_DETAIL_PRECOMPILE_READERS(BOND_DETAIL_EXTERN_BasicTypeField_Null)


#define BOND_DETAIL_INSTANTIATE_BasicTypeContainer(Reader, Writer) \
    template void BasicTypeContainer<BuiltInProtocols>(const Serializer<Writer>&, BondDataType, Reader&, uint32_t);

#define BOND_DETAIL_EXTERN_BasicTypeContainer(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_BasicTypeContainer(Reader, Writer)

BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_EXTERN_BasicTypeContainer)


#if defined(_MSC_VER) && (_MSC_VER < 1900)
#define BOND_DETAIL_INSTANTIATE_MapByKey(Reader, Writer, T) \
    template void MapByKey<BuiltInProtocols, const Serializer<Writer>, value<T, Reader&> >( \
        const Serializer<Writer>&, BondDataType, const value<T, Reader&>&, Reader&, uint32_t);
#else
#define BOND_DETAIL_INSTANTIATE_MapByKey(Reader, Writer, T) \
    template void MapByKey<BuiltInProtocols>( \
        const Serializer<Writer>&, BondDataType, const value<T, Reader&>&, Reader&, uint32_t);
#endif

#define BOND_DETAIL_EXTERN_MapByKey(Reader, Writer, T) \
    extern BOND_DETAIL_INSTANTIATE_MapByKey(Reader, Writer, T)

BOND_DETAIL_PRECOMPILE_READERS_WRITERS_BASIC_TYPES(BOND_DETAIL_EXTERN_MapByKey)


#define BOND_DETAIL_INSTANTIATE_MapByElement(Reader, Writer) \
    template void MapByElement<BuiltInProtocols>( \
        const Serializer<Writer>&, BondDataType, BondDataType, Reader&, uint32_t);

#define BOND_DETAIL_EXTERN_MapByElement(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_MapByElement(Reader, Writer)

BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_EXTERN_MapByElement)


} // namespace detail
} // namespace bond
