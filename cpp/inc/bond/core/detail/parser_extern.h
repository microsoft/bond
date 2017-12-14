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
    (BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY)
    (BOND_DETAIL_BUILTIN_WRITERS))


#define BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields_Null(Reader) \
    template bool StaticParser<Reader&>::ReadFields(const RuntimeSchema&, const Null&);

#define BOND_DETAIL_EXTERN_StaticParser_ReadFields_Null(Reader) \
    extern BOND_DETAIL_INSTANTIATE_StaticParser_ReadFields_Null(Reader)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_StaticParser_ReadFields_Null,
    (BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY))


#define BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields(Reader, Writer) \
    template bool DynamicParser<Reader&>::ReadFields(const RuntimeSchema&, const Serializer<Writer>&);

#define BOND_DETAIL_EXTERN_DynamicParser_ReadFields(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields(Reader, Writer)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_DynamicParser_ReadFields,
    (BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOND_DETAIL_BUILTIN_READER_FAST_BINARY)
    (BOND_DETAIL_BUILTIN_WRITERS))


#define BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields_Null(Reader) \
    template bool DynamicParser<Reader&>::ReadFields(const RuntimeSchema&, const Null&);

#define BOND_DETAIL_EXTERN_DynamicParser_ReadFields_Null(Reader) \
    extern BOND_DETAIL_INSTANTIATE_DynamicParser_ReadFields_Null(Reader)

BOND_DETAIL_PRECOMPILE(
    BOND_DETAIL_EXTERN_DynamicParser_ReadFields_Null,
    (BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOND_DETAIL_BUILTIN_READER_FAST_BINARY))


} // namespace bond
