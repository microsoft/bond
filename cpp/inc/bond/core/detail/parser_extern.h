// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include "extern_macro.h"


namespace bond
{

#define BOND_DETAIL_StaticParser_ReadFields(Reader, Writer) \
    bool StaticParser<Reader&>::ReadFields(const RuntimeSchema&, const Serializer<Writer>&);

BOND_DETAIL_EXTERN(
    BOND_DETAIL_StaticParser_ReadFields,
    (BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY)
    BOND_DETAIL_BUILTIN_WRITERS)


#define BOND_DETAIL_StaticParser_ReadFields_Null(Reader) \
    bool StaticParser<Reader&>::ReadFields(const RuntimeSchema&, const Null&);

BOND_DETAIL_EXTERN(
    BOND_DETAIL_StaticParser_ReadFields_Null,
    (BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY))


#define BOND_DETAIL_DynamicParser_ReadFields(Reader, Writer) \
    bool DynamicParser<Reader&>::ReadFields(const RuntimeSchema&, const Serializer<Writer>&);

BOND_DETAIL_EXTERN(
    BOND_DETAIL_DynamicParser_ReadFields,
    (BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOND_DETAIL_BUILTIN_READER_FAST_BINARY)
    BOND_DETAIL_BUILTIN_WRITERS)


#define BOND_DETAIL_DynamicParser_ReadFields_Null(Reader) \
    bool DynamicParser<Reader&>::ReadFields(const RuntimeSchema&, const Null&);

BOND_DETAIL_EXTERN(
    BOND_DETAIL_DynamicParser_ReadFields_Null,
    (BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOND_DETAIL_BUILTIN_READER_FAST_BINARY))


} // namespace bond
