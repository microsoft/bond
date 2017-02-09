//
// This is example of logging handler implementation
//

//
// Enable bond.comm logging
// 
#define BOND_ENABLE_LOG_HANDLER

// Include auto-generated files.
#include "logging_reflection.h"
#include "logging_comm.h"

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

#include <future>
#include <iomanip>


static std::mutex log_mutex;
namespace bond { namespace comm
{

void LogHandler(const char* functionName,
                const char* fileName,
                uint32_t lineNumber,
                LogSeverity severity,
                const char* category,
                const char* message)
{
    const std::thread::id t_id = std::this_thread::get_id();

    char level = 'x';
    switch (severity)
    {
        case LOG_TRACE: level = 't'; break;
        case LOG_DEBUG: level = 'd'; break;
        case LOG_INFO: level = 'i'; break;
        case LOG_WARNING: level = 'w'; break;
        case LOG_ERROR: level = 'e'; break;
        case LOG_FATAL: level = 'f'; break;
    }

    const char* fn = strrchr(fileName, '/');
    if (nullptr == fn)
    {
        fn = fileName;
    }
    else
    {
        ++fn;
    }

    std::unique_lock<std::mutex> ul(log_mutex);
    std::clog << level << ','
              << category << ", "
              << message 
              << " t=" << t_id
              << " [" << functionName << " " << fn << "(" << lineNumber << ")]\n";
}
} } // namespace bond.comm


using namespace examples::logging;

// Implement service 
class ServiceImpl : public Service
{
    void Method(const bond::comm::payload<void>&,
                const std::function<void(const bond::comm::message<void>&)>& callback) override
    {
        // Respond right away, with void message
        callback(bond::comm::message<void>());
    }
};


int BOND_CALL main()
{
    bond::comm::epoxy::EpoxyTransport transport;

    bond::comm::SocketAddress loopback("127.0.0.1", EXAMPLE_PORT_1);
    auto server = transport.Bind(loopback, boost::make_shared<ServiceImpl>());

    Service::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

    // Invoke proxy...
    proxy.Method().get().value();

    return 0;
}
