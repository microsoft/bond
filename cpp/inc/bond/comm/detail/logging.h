#pragma once

#include <bond/core/detail/string_stream.h>

namespace bond { namespace comm
{

enum LogSeverity
{
    LOG_TRACE = 0,
    LOG_DEBUG = 1,
    LOG_INFO = 2,
    LOG_WARNING = 3,
    LOG_ERROR = 4,
    LOG_FATAL = 5,
};


void LogHandler(const char* functionName,
                const char* fileName,
                uint32_t lineNumber,
                LogSeverity severity,
                const char* category,
                const char* message);

} } // namespace bond::comm

//
// Convenient define for Log messages.
//
#if !defined(BOND_LOG)
#   ifdef BOND_ENABLE_LOG_HANDLER
#       define BOND_LOG(severity, category, message)  \
            bond::comm::LogHandler(__FUNCTION__, __FILE__, __LINE__, severity, category, (bond::detail::string_stream() << message).content());
#   else
#       define BOND_LOG(severity, category, message) if (0) { (bond::detail::string_stream() << message).content(); }
#   endif
#endif
