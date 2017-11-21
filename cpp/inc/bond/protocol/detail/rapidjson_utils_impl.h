// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>
#include <bond/core/exception.h>

#include <boost/lexical_cast.hpp>
#include <boost/locale.hpp>

namespace bond
{
namespace detail
{

BOND_DETAIL_HEADER_ONLY_INLINE
std::basic_string<uint16_t> utf_to_utf(const char* begin, const char* end)
{
    try
    {
        return boost::locale::conv::utf_to_utf<uint16_t>(begin, end, boost::locale::conv::stop);
    }
    catch (const boost::locale::conv::conversion_error&)
    {
        UnicodeConversionException();
    }
}


BOND_DETAIL_HEADER_ONLY_INLINE
bool try_lexical_convert(const char* str, uint16_t& result)
{
    return boost::conversion::try_lexical_convert(str, result);
}


} // namespace detail

} // namespace bond
