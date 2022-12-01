// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <stdint.h>
#include <type_traits>

#define BOND_VERSION 0x0b00
#define BOND_MIN_CODEGEN_VERSION 0x0d00

namespace bond
{
    template <typename... T>
    struct Protocols;

    template <typename BufferT>
    class CompactBinaryReader;

    template <typename BufferT, typename MarshaledBondedProtocolsT = Protocols<CompactBinaryReader<BufferT> > >
    class SimpleBinaryReader;

    BOND_CONSTEXPR_OR_CONST uint16_t v1 = 0x0001;
    BOND_CONSTEXPR_OR_CONST uint16_t v2 = 0x0002;

    template <typename T> struct
    default_version
        : std::integral_constant<uint16_t, v1> {};
}
