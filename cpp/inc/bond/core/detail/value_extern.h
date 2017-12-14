// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include "extern_macro.h"


namespace bond
{

#define BOND_DETAIL_ValueVoid_Apply(Reader, Writer) \
    void value<void, Reader&>::_Apply(const Serializer<Writer>&) const;

BOND_DETAIL_EXTERN(BOND_DETAIL_ValueVoid_Apply, BOND_DETAIL_BUILTIN_READERS_WRITERS)


} // namespace bond
