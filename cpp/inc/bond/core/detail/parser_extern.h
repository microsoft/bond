// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "extern_macro.h"


namespace bond
{

#define BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields(Reader, Writer) \
    template bool StaticParser<Reader&>::ReadFields(const RuntimeSchema&, const Serializer<Writer>&);

#define BOND_DETAIL_EXTERN_StaticParser_ReadFields(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields(Reader, Writer)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_StaticParser_ReadFields,
    ((SimpleBinaryReader<InputBuffer>))
    (BOND_DETAIL_BUILTIN_WRITERS))


#define BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields_Null(Reader) \
    template bool StaticParser<Reader&>::ReadFields(const RuntimeSchema&, const Null&);

#define BOND_DETAIL_EXTERN_StaticParser_ReadFields_Null(Reader) \
    extern BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields_Null(Reader)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_StaticParser_ReadFields_Null,
    ((SimpleBinaryReader<InputBuffer>)))


#define BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields(Reader, Writer) \
    template bool DynamicParser<Reader&>::ReadFields(const RuntimeSchema&, const Serializer<Writer>&);

#define BOND_DETAIL_EXTERN_DynamicParser_ReadFields(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields(Reader, Writer)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_DynamicParser_ReadFields,
    ((CompactBinaryReader<InputBuffer>)(FastBinaryReader<InputBuffer>))
    (BOND_DETAIL_BUILTIN_WRITERS))


#define BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields_Null(Reader) \
    template bool DynamicParser<Reader&>::ReadFields(const RuntimeSchema&, const Null&);

#define BOND_DETAIL_EXTERN_DynamicParser_ReadFields_Null(Reader) \
    extern BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields_Null(Reader)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_DynamicParser_ReadFields_Null,
    ((CompactBinaryReader<InputBuffer>)(FastBinaryReader<InputBuffer>)))


} // namespace bond
