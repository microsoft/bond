// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#ifndef _CRT_SECURE_NO_WARNINGS
#   define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <stdlib.h>

#include <bond/core/bond.h>
#include <bond/core/cmdargs.h>
#include <bond/stream/stdio_output_stream.h>

#include <boost/algorithm/string/replace.hpp>

#include "commcmd_arg_types.h"
#include "commcmd_arg_reflection.h"

#define die(ARGS) printf(ARGS); exit(1);

using namespace unittest::compat;

FILE* popen_wrapper(const char* command, const char* mode)
{
#ifdef _WIN32
    std::string escaped_command = command;
    boost::replace_all(escaped_command, " ", "\" \"");
    return _popen(escaped_command.c_str(), mode);
#else
    return popen(command, mode);
#endif
}

int pclose_wrapper(FILE* stream)
{
#ifdef _WIN32
    return _pclose(stream);
#else
    return pclose(stream);
#endif
}

int main(int argc, char** argv)
{
    Options options;

    try
    {
        options = bond::cmd::GetArgs<Options>(argc, argv);
    }
    catch(const std::exception& e)
    {
        printf("\n%s\n", e.what());
        options.help = true;
    }

    if (options.help)
    {
        bond::cmd::ShowUsage<Options>(argv[0]);
        exit(1);
    }
    else if (options.server_exe.empty())
    {
        die("Server exe must be provided");
    }
    else if (options.client_exe.empty())
    {
        die("Client exe must be provided");
    }

    FILE *server_process;

    printf("Spawning server\n");

    if ((server_process = popen_wrapper(options.server_exe.c_str(), "r")) == NULL)
    {
        printf("popen of server process failed\n");
        exit(1);
    }

    bool server_ok = false;
    bool client_ok = false;

    char server_buffer[128];

    if (fgets(server_buffer, 128, server_process))
    {
        printf("Server status: %s\n", server_buffer);
        if (strcmp("Server ready\n", server_buffer) != 0)
        {
            exit(1);
        }
    }

    printf("Moving to client\n");

    FILE *client_process;

    if ((client_process = popen_wrapper(options.client_exe.c_str(), "r")) == NULL)
    {
        printf("popen of client process failed\n");
        exit(1);
    }

    char client_buffer[128];

    while (fgets(client_buffer, 128, client_process))
    {
        printf("Client status: %s\n", client_buffer);
        if (strcmp("Client succeeded\n", client_buffer) == 0)
        {
            client_ok = true;
        }
    }

    while (fgets(server_buffer, 128, server_process))
    {
        printf("Server status: %s\n", server_buffer);
        if (strcmp("Server completed\n", server_buffer) == 0)
        {
            server_ok = true;
        }
    }

    pclose_wrapper(server_process);
    pclose_wrapper(client_process);

    if (!server_ok || !client_ok)
    {
        exit(1);
    }

    return 0;    
}
