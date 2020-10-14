#ifndef _CRT_SECURE_NO_WARNINGS
#   define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdarg.h>
#include <stdio.h>

#include "compat_reflection.h"
#include "compat_no_generics_reflection.h"
#include "compat.h"
#include "compare.h"

#include "cmd_arg_reflection.h"
#include <bond/core/cmdargs.h>

#include <bond/core/blob.h>
#include <bond/protocol/simple_json_writer.h>
#include <bond/stream/stdio_output_stream.h>

[[noreturn]] void die(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);

    exit(1);
}

template <typename T>
void Render(const T& obj, FILE* file)
{
    bond::StdioOutputStream out(file);
    bond::SimpleJsonWriter<bond::StdioOutputStream> writer(out, true);
    bond::Serialize(obj, writer);
}

template <typename T>
void Verify(Options options, const T& obj, const T& obj2)
{
    // custom comparison which isn't sentitive to precision loss when serializing
    // double to base 10 text format
    if (!Equal(obj, obj2))
    {
        if (!options.expected.empty() && !options.actual.empty())
        {
            FILE* file1 = fopen(options.expected.c_str(), "w");
            if (file1 == nullptr)
            {
                die("\nCan't open %s\n", options.expected.c_str());
            }

            FILE* file2 = fopen(options.actual.c_str(), "w");
            if (file2 == nullptr)
            {
                die("\nCan't open %s\n", options.actual.c_str());
            }

            Render(obj, file1);
            Render(obj2, file2);
        }

        die("\nThe deserialized object doesn't match\n");
    }
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
        fprintf(stderr, "\n%s\n", e.what());
        options.help = true;
    }

    if (options.help || (options.deserialize.empty() == options.serialize.empty()))
    {
        bond::cmd::ShowUsage<Options>(argv[0]);
    }
    else if (!options.deserialize.empty())
    {
        void* buffer = malloc(MAX_SIZE);
        if (buffer == nullptr)
        {
            die("\nFailed to allocate memory\n");
        }

        FILE* file = fopen(options.deserialize.c_str(), "rb");
        if (file == nullptr)
        {
            die("\nCan't open %s\n", options.deserialize.c_str());
        }

        size_t size = fread(buffer, 1, MAX_SIZE, file);

        Compat obj;
        Init(options.test, obj);

        Compat obj2;
        bond::blob input(buffer, static_cast<uint32_t>(size));

        bond::SchemaDef schema2;

        Deserialize(options.test, input, obj2, schema2);

        if (!options.no_generics)
        {
            if (options.test == schema)
            {
                Verify(options, bond::GetRuntimeSchema<Compat>().GetSchema(), schema2);
            }
            else
            {
                Verify(options, obj, obj2);
            }
        }
        else
        {
            if (options.test == schema)
            {
                Verify(options, bond::GetRuntimeSchema<CompatNoGenerics>().GetSchema(), schema2);
            }
            else
            {
                CompatNoGenerics no_generics, no_generics2;

                Convert(obj, no_generics);
                Convert(obj2, no_generics2);

                Verify(options, no_generics, no_generics2);
            }
        }
    }
    else if (!options.serialize.empty())
    {
        Compat obj;
        Init(options.test, obj);

        bond::blob output = Serialize(options.test, obj);

        FILE* file = fopen(options.serialize.c_str(), "wb");
        if (file == nullptr)
        {
            die("Can't open %s\n", options.serialize.c_str());
        }

        fwrite(output.data(), 1, output.size(), file);

        if (output.size() >= MAX_SIZE)
        {
            die("\nSerialized object is too big\n");
        }
    }

    return 0;
}
