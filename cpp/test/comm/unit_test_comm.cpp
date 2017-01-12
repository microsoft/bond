
#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#ifdef _MSC_VER
    #pragma warning(disable : 4505) // disable "unreferenced local function has been removed" warning
#endif

#include <unit_test_comm_reflection.h>
#include <unit_test_comm_comm.h>

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

void init_unit_test_comm_core();
void init_unit_test_comm_event();
void init_unit_test_comm_protocol();
void init_unit_test_comm_utils();
void init_unit_test_comm_basic();
void init_unit_test_comm_void();
void init_unit_test_comm_layers();
void init_unit_test_comm_jumbo();
void init_unit_test_comm_session();
void init_unit_test_comm_timeout();
void init_unit_test_comm_forward();
void init_unit_test_comm_generic_type();
void init_unit_test_comm_generic_service();
void init_unit_test_comm_error();
void init_unit_test_comm_async();
void init_unit_test_comm_unexpected();

bool init_unit_test()
{
    init_unit_test_comm_utils();
    init_unit_test_comm_core();
    init_unit_test_comm_basic();
    init_unit_test_comm_void();
    init_unit_test_comm_layers();
    init_unit_test_comm_jumbo();
    init_unit_test_comm_session();
    init_unit_test_comm_timeout();
    init_unit_test_comm_forward();
    init_unit_test_comm_generic_type();
    init_unit_test_comm_generic_service();
    init_unit_test_comm_event();
    init_unit_test_comm_protocol();
    init_unit_test_comm_error();
    init_unit_test_comm_async();
    init_unit_test_comm_unexpected();

    return true;
}
