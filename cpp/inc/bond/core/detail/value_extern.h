// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "extern_macro.h"


namespace bond
{

#define BOND_DETAIL_INSTANTIATE_ValueVoid_Apply(Reader, Writer) \
    template void value<void, Reader&>::_Apply(const Serializer<Writer>&) const;

#define BOND_DETAIL_EXTERN_ValueVoid_Apply(Reader, Writer) \
    extern BOND_DETAIL_INSTANTIATE_ValueVoid_Apply(Reader, Writer)

BOND_DETAIL_PRECOMPILE_READERS_WRITERS(BOND_DETAIL_EXTERN_ValueVoid_Apply)


} // namespace bond
