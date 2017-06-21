#pragma once

#include <string.h>
#include <string>

// Convert an error number into a human-readable string.
inline std::string ErrorString(int errnum)
{
    std::string result;

#ifdef _MSC_VER
    // strerrorlen_s hasn't made it into the Microsoft C runtime yet. Until
    // it has, we're just going to reserve a buffer that's pretty big and
    // get as much of the error message as we can.
    result.reserve(240);

    (void)strerror_s(&result[0], result.size(), errnum);

    // strerror_s wrote an embedded NUL, so truncate to that size so the
    // result doesn't have an embedded NUL.
    result.resize(strlen(result.c_str()));
#else
    // strerror isn't guaranteed to be thread-safe, but bf is single
    // threaded.
    result = strerror(errnum);
#endif

    return result;
}
