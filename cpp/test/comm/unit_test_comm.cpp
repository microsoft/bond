
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <comm_test_common_reflection.h>
#include <comm_test_common_comm.h>

#include "transport_list.h"
#include <bond/comm/layers.h>
#include <bond/comm/timeout.h>
#include <bond/comm/thread_pool.h>

#include <boost/mpl/for_each.hpp>

// TODO: move unit_test_framework.h to cpp/test/inc
#include "../core/unit_test_framework.h"
#include "bonded_cast.h"
#include "test_utils_comm.h"
#include <boost/chrono.hpp>
#include <string.h>
#include <atomic>

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

void init_core();
void init_event();
void init_protocol();
void init_utils();
void init_basic();
void init_void();
void init_layers();
void init_jumbo();
void init_session();
void init_timeout();
void init_forward();
void init_generic_type();
void init_generic_service();
void init_error();
void init_async();
void init_unexpected();

bool init_unit_test()
{
    init_utils();
    init_core();
    init_basic();
    init_void();
    init_layers();
    init_jumbo();
    init_session();
    init_timeout();
    init_forward();
    init_generic_type();
    init_generic_service();
    init_event();
    init_protocol();
    init_error();
    init_async();
    init_unexpected();

    return true;
}
