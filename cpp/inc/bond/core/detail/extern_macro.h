// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#if !defined(BOND_COMPACT_BINARY_PROTOCOL) \
 && !defined(BOND_SIMPLE_BINARY_PROTOCOL) \
 && !defined(BOND_FAST_BINARY_PROTOCOL) \
 && !defined(BOND_SIMPLE_JSON_PROTOCOL)
#error None of the built-in protocols are enabled. Either enable one of them or set BOND_LIB_TYPE to BOND_LIB_TYPE_HEADER.
#endif

#include <boost/preprocessor/facilities/expand.hpp>
#include <boost/preprocessor/seq/for_each_product.hpp>
#include <boost/preprocessor/seq/pop_front.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/to_tuple.hpp>


#ifdef BOND_COMPACT_BINARY_PROTOCOL
#define BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY (bond::CompactBinaryReader<bond::InputBuffer>)
#define BOND_DETAIL_BUILTIN_WRITER_COMPACT_BINARY (bond::CompactBinaryWriter<bond::OutputBuffer>) \
                                                  (bond::CompactBinaryWriter<bond::OutputBuffer>::Pass0)
#else
#define BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY BOOST_PP_SEQ_NIL
#define BOND_DETAIL_BUILTIN_WRITER_COMPACT_BINARY BOOST_PP_SEQ_NIL
#endif

#ifdef BOND_SIMPLE_BINARY_PROTOCOL
#define BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY (bond::SimpleBinaryReader<bond::InputBuffer>)
#define BOND_DETAIL_BUILTIN_WRITER_SIMPLE_BINARY (bond::SimpleBinaryWriter<bond::OutputBuffer>)
#else
#define BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY BOOST_PP_SEQ_NIL
#define BOND_DETAIL_BUILTIN_WRITER_SIMPLE_BINARY BOOST_PP_SEQ_NIL
#endif

#ifdef BOND_FAST_BINARY_PROTOCOL
#define BOND_DETAIL_BUILTIN_READER_FAST_BINARY (bond::FastBinaryReader<bond::InputBuffer>)
#define BOND_DETAIL_BUILTIN_WRITER_FAST_BINARY (bond::FastBinaryWriter<bond::OutputBuffer>)
#else
#define BOND_DETAIL_BUILTIN_READER_FAST_BINARY BOOST_PP_SEQ_NIL
#define BOND_DETAIL_BUILTIN_WRITER_FAST_BINARY BOOST_PP_SEQ_NIL
#endif

#ifdef BOND_SIMPLE_JSON_PROTOCOL
#define BOND_DETAIL_BUILTIN_READER_SIMPLE_JSON (bond::SimpleJsonReader<bond::InputBuffer>)
#define BOND_DETAIL_BUILTIN_WRITER_SIMPLE_JSON (bond::SimpleJsonWriter<bond::OutputBuffer>)
#else
#define BOND_DETAIL_BUILTIN_READER_SIMPLE_JSON BOOST_PP_SEQ_NIL
#define BOND_DETAIL_BUILTIN_WRITER_SIMPLE_JSON BOOST_PP_SEQ_NIL
#endif

#define BOND_DETAIL_BUILTIN_READERS \
    ( \
    BOND_DETAIL_BUILTIN_READER_COMPACT_BINARY \
    BOND_DETAIL_BUILTIN_READER_SIMPLE_BINARY \
    BOND_DETAIL_BUILTIN_READER_FAST_BINARY \
    /*BOND_DETAIL_BUILTIN_READER_SIMPLE_JSON*/ \
    )

#define BOND_DETAIL_BUILTIN_WRITERS \
    ( \
    BOND_DETAIL_BUILTIN_WRITER_COMPACT_BINARY \
    BOND_DETAIL_BUILTIN_WRITER_SIMPLE_BINARY \
    BOND_DETAIL_BUILTIN_WRITER_FAST_BINARY \
    /*BOND_DETAIL_BUILTIN_WRITER_SIMPLE_JSON*/ \
    )

#define BOND_DETAIL_BUILTIN_BASIC_TYPES \
    ( \
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
    (int64_t) \
    )

#define BOND_DETAIL_BUILTIN_READERS_WRITERS BOND_DETAIL_BUILTIN_READERS BOND_DETAIL_BUILTIN_WRITERS

#define BOND_DETAIL_BUILTIN_READERS_WRITERS_BASIC_TYPES \
    BOND_DETAIL_BUILTIN_READERS BOND_DETAIL_BUILTIN_WRITERS BOND_DETAIL_BUILTIN_BASIC_TYPES


#define BOND_DETAIL_APPLY_FUNC(_, product) \
    BOOST_PP_EXPAND(BOOST_PP_SEQ_HEAD(product) BOOST_PP_EXPAND(BOOST_PP_SEQ_TO_TUPLE(BOOST_PP_SEQ_POP_FRONT(product))))

#define BOND_DETAIL_DECLARE(func, seq) BOOST_PP_SEQ_FOR_EACH_PRODUCT(BOND_DETAIL_APPLY_FUNC, ((func))seq)

#define BOND_DETAIL_PREFIX_TEMPLATE(func) template func
#define BOND_DETAIL_INSTANTIATE(func, seq) BOND_DETAIL_DECLARE(BOND_DETAIL_PREFIX_TEMPLATE(func), seq)

#define BOND_DETAIL_PREFIX_EXTERN(func) extern BOND_DETAIL_PREFIX_TEMPLATE(func)
#define BOND_DETAIL_EXTERN(func, seq) BOND_DETAIL_DECLARE(BOND_DETAIL_PREFIX_EXTERN(func), seq)
