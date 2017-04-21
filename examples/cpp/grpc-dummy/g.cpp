#include "g_types.h"

#pragma warning (push)
#pragma warning (disable: 4100)

#include <grpc++/grpc++.h>

#pragma warning (pop)

#include <stdio.h>

using namespace g;

int main()
{
    Box b;
    b.sf = "world";

    printf("Hello, %s\n", b.sf.c_str());

    grpc::ServerBuilder sb;
    sb.AddListeningPort("127.0.0.1:12345", nullptr);

    return 0;
}
