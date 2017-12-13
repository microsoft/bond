// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/pop_front.hpp>
#include <boost/preprocessor/seq/to_tuple.hpp>
#include <boost/preprocessor/seq/for_each_product.hpp>
#include <boost/preprocessor/facilities/expand.hpp>


#define BOND_DETAIL_APPLY_FUNC(r, p) \
    BOOST_PP_EXPAND(BOOST_PP_SEQ_HEAD(p) BOOST_PP_EXPAND(BOOST_PP_SEQ_TO_TUPLE(BOOST_PP_SEQ_POP_FRONT(p))))

#define BOND_DETAIL_PRECOMPILE(F, S) \
    BOOST_PP_SEQ_FOR_EACH_PRODUCT(BOND_DETAIL_APPLY_FUNC, ((F))S)

#define BOND_DETAIL_BUILTIN_READERS \
    (CompactBinaryReader<InputBuffer>) \
    (SimpleBinaryReader<InputBuffer>) \
    (FastBinaryReader<InputBuffer>)

#define BOND_DETAIL_BUILTIN_WRITERS \
    (CompactBinaryWriter<OutputBuffer>) \
    (CompactBinaryWriter<OutputBuffer>::Pass0) \
    (SimpleBinaryWriter<OutputBuffer>) \
    (FastBinaryWriter<OutputBuffer>)

#define BOND_DETAIL_BUILTIN_BASIC_TYPES \
    (void) \
    (bool) \
    (uint8_t) \
    (uint16_t) \
    (uint32_t) \
    (uint64_t) \
    (float) \
    (double) \
    (std::string) \
    (std::wstring) \
    (int8_t) \
    (int16_t) \
    (int32_t) \
    (int64_t)

#define BOND_DETAIL_PRECOMPILE_READERS(F) \
    BOND_DETAIL_PRECOMPILE(F, (BOND_DETAIL_BUILTIN_READERS))

#define BOND_DETAIL_PRECOMPILE_WRITERS(F) \
    BOND_DETAIL_PRECOMPILE(F, (BOND_DETAIL_BUILTIN_WRITERS))

#define BOND_DETAIL_PRECOMPILE_READERS_WRITERS(F) \
    BOND_DETAIL_PRECOMPILE(F, (BOND_DETAIL_BUILTIN_READERS)(BOND_DETAIL_BUILTIN_WRITERS))

#define BOND_DETAIL_PRECOMPILE_READERS_WRITERS_BASIC_TYPES(F) \
    BOND_DETAIL_PRECOMPILE(F, (BOND_DETAIL_BUILTIN_READERS)(BOND_DETAIL_BUILTIN_WRITERS)(BOND_DETAIL_BUILTIN_BASIC_TYPES))
