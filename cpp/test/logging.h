#include <bond/comm/detail/logging.h>

#define BOND_ENABLE_LOG_HANDLER

namespace bond { namespace comm
{

void LogHandler(const char* functionName,
                const char* fileName,
                uint32_t lineNumber,
                LogSeverity /*severity*/,
                const char* category,
                const char* message)
{
#if defined(_MSC_VER)

    OutputDebugStringA((bond::detail::string_stream() <<
        category << ", "<< message <<
        " [" << functionName << " " << fileName << "(" << lineNumber << ") " << "]\n"
    ).content());

#else

    //bond::detail::string_stream ss;
    //ss << category << ", "
    //   << message << " ["
    //   << functionName << " "
    //   << fileName << "("
    //   << lineNumber << ") ]\n";
    //puts(ss.content());
    //BOOST_TEST_MESSAGE(ss.content());

#endif
}
} } // namespace bond.comm
