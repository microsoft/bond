#pragma once

#include <string.h>
#include <string>

// Convert an errno_t into a human-readable string.
inline std::string ErrorString(errno_t errnum)
{
    // strerrorlen_s hasn't made it into the Microsoft C runtime yet. Until
    // it has, we're just going to reserve a buffer that's pretty big and
    // get as much of the error message as we can.
    std::string result;
    result.reserve(240);

    (void)strerror_s(&result[0], result.size(), errnum);

    // strerror_s wrote an embedded NUL, so truncate to that size so the
    // result doesn't have an embedded NUL
    result.resize(strlen(result.c_str()));
    return result;
}
