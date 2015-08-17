#pragma once

#include <bond/core/scalar_interface.h>
#include <boost/date_time/posix_time/posix_time.hpp>

namespace bond
{
    template <> struct
    aliased_type<boost::posix_time::ptime>
    {
        typedef int64_t type;
    };
}


namespace time_consts
{
    using boost::posix_time::time_duration;

    // Ticks in .NET's System.DateTime value for boost::posix_time::min_date_time
    static const int64_t min_date_time_ticks = 441481536000000000LL;

    // Resolution used to encode time on the wire (100ns)
    static const int64_t wire_ticks_per_second = 10000000LL;

    static const int64_t res_adjust_up =
        wire_ticks_per_second < time_duration::ticks_per_second()
      ? 1 : wire_ticks_per_second / time_duration::ticks_per_second();

    static const int64_t res_adjust_down =
        wire_ticks_per_second > time_duration::ticks_per_second()
      ? 1 : time_duration::ticks_per_second() / wire_ticks_per_second;

    static const boost::posix_time::ptime min_date_time = boost::posix_time::min_date_time;
}


// Set ptime from value representing number of 100ns ticks since January 1, 1 AD
inline
void set_aliased_value(boost::posix_time::ptime& var, int64_t value)
{
    using boost::posix_time::time_duration;

    var = value ?
        time_consts::min_date_time + time_duration(0, 0, 0,
            (value - time_consts::min_date_time_ticks)
            * time_consts::res_adjust_down
            / time_consts::res_adjust_up) :
        boost::posix_time::not_a_date_time;
}


// Return number of 100ns ticks since January 1, 1 AD that represent the ptime value
inline
int64_t get_aliased_value(const boost::posix_time::ptime& value)
{
    if (value.is_not_a_date_time())
        return 0;

    return (value - time_consts::min_date_time).ticks()
        * time_consts::res_adjust_up
        / time_consts::res_adjust_down
        + time_consts::min_date_time_ticks;
}

