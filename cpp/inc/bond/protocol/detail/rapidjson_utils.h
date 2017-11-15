// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <string>
#include <stdint.h>


namespace bond
{
namespace detail
{

std::basic_string<uint16_t> utf_to_utf(const char* begin, const char* end);

bool try_lexical_convert(const char* str, uint16_t& result);


} // namespace detail

} // namespace bond


#ifdef BOND_HEADER_ONLY
#include "rapidjson_utils_impl.h"
#endif
