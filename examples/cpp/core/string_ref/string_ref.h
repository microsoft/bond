#pragma once

#include <boost/utility/string_ref.hpp>
#include <bond/core/container_interface.h>
#include <boost/numeric/conversion/cast.hpp>
#include <boost/assert.hpp>

namespace bond
{
    template <> struct 
    is_string<boost::string_ref>
        : std::true_type {};

    inline
    const char* string_data(const boost::string_ref& str)
    {
        return str.data();
    }

    inline
    char* string_data(boost::string_ref&)
    {
        // string_ref is readonly
        BOOST_ASSERT(false);
        return 0;
    }

    inline
    uint32_t string_length(const boost::string_ref& str)
    {
        return boost::numeric_cast<uint32_t>(str.length());
    }

    inline
    void resize_string(boost::string_ref&, uint32_t)
    {
        // string_ref is readonly
        BOOST_ASSERT(false);
    }
}

