
#pragma once

#include <bond/comm/comm_types.h>
#include <exception>

namespace bond { namespace comm
{

struct Request
{
    std::string service_name;

    std::string method_name;

    std::vector<blob> layers;
    
    std::vector<blob> payload;
};

typedef Request Event;

struct Response
{
    Error error;

    std::vector<blob> layers;

    std::vector<blob> payload;

    bool is_error;
};

} } // namespace bond.comm
