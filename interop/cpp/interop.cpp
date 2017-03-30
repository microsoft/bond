#include <stdio.h>
#include <string.h>

#include <bond/core/bond.h>
#include <bond/stream/stdio_output_stream.h>

#include "gen/interop_reflection.h"
#include "gen/interop_apply.h"

using namespace com::microsoft::bond::interop;

int main(int argc, char ** argv) {
    if (argc != 3) {
        printf("Usage: interop read|write file\n");
        return 255;
    }

    bool read_mode = false;
    if (strcmp(argv[1], "read") == 0) {
        read_mode = true;
    } else if (strcmp(argv[1], "write") == 0) {
        read_mode = false;
    } else {
        printf("Unknown mode: %s\n", argv[1]);
        return 255;
    }

    char * fpath = argv[2];

    if (read_mode) {
        const size_t buf_len = 1024 * 1024;
        void* buffer = malloc(buf_len);
        FILE* d = fopen(fpath, "rb");
        size_t size = fread(buffer, 1, buf_len, d);
        fclose(d);

        if (size == buf_len) {
            printf("Buffer was completely filled with serialized data.");
            printf("It is very likely that you didn't read the full object!");
        }

        OneOfEverything ooe;
        bond::InputBuffer input(buffer, size);
        Unmarshal(input, ooe);

        printf("ooe.b: %d\n", ooe.b);
        printf("ooe.s: %d\n", ooe.s);
        printf("ooe.i: %d\n", ooe.i);
        printf("ooe.l: %ld\n", ooe.l);
        printf("ooe.ub: %u\n", ooe.ub);
        printf("ooe.us: %u\n", ooe.us);
        printf("ooe.ui: %u\n", ooe.ui);
        printf("ooe.ul: %lu\n", ooe.ul);
        printf("ooe.f: %f\n", ooe.f);
        printf("ooe.d: %f\n", ooe.d);
        printf("ooe.bl: %s\n", ooe.bl ? "true" : "false");
        printf("ooe.str: %s\n", ooe.str.c_str());
        printf("ooe.wstr: %S\n", ooe.wstr.c_str());
    } else {
        OneOfEverything ooe;
        ooe.b = 3;
        ooe.s = 333;
        ooe.i = 333333;
        ooe.l = 33333333333;
        ooe.ub = 128;
        ooe.us = 32768;
        ooe.ui = 2147483648;
        // 9223372036854775808, in hex, to avoid a warning.
        ooe.ul = 0x8000000000000000;
        ooe.f = 3.3;
        ooe.d = 3.3;
        ooe.bl = true;
        ooe.str = "three";
        ooe.wstr = L"threeeee";

        FILE* d = fopen(fpath, "wb");
        bond::StdioOutputStream output(d);
        bond::FastBinaryWriter<bond::StdioOutputStream> writer(output);
        Marshal(ooe, writer);
        fclose(d);
    }
    return 0;
}
