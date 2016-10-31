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

#include "commcmd_arg_types.h"
#include "commcmd_arg_reflection.h"

#define die(ARGS) (printf ARGS, exit(1), true);

using namespace unittest::compat;

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
        die(("Server exe must be provided"));
    }
    else if (options.client_exe.empty())
    {
        die(("Client exe must be provided"));
    }

    FILE *server_process;

    printf("Spawning server\n");

    if ((server_process = _popen(options.server_exe.c_str(), "rt")) == NULL)
    {
        printf("_popen of server process failed");
        exit(1);
    }

    bool server_ok = false;
    bool client_ok = false;

    char   server_buffer[128];

    if (fgets(server_buffer, 128, server_process))
    {
        printf(server_buffer);
        if (strcmp("Server ready\n", server_buffer) != 0)
        {
            exit(1);
        }
    }

    printf("Moving to client\n");

    FILE *client_process;

    if ((client_process = _popen(options.client_exe.c_str(), "rt")) == NULL)
    {
        printf("_popen of client process failed");
        exit(1);
    }

    char   client_buffer[128];

    while (fgets(client_buffer, 128, client_process))
    {
        printf(client_buffer);
        if (strcmp("Client succeeded\n", client_buffer) == 0)
        {
            client_ok = true;
        }
    }

    while (fgets(server_buffer, 128, server_process))
    {
        printf(server_buffer);
        if (strcmp("Server completed\n", server_buffer) == 0)
        {
            server_ok = true;
        }
    }

    _pclose(server_process);
    _pclose(client_process);

    if (!server_ok || !client_ok)
    {
        exit(1);
    }

    return 0;    
}
