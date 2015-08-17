#include "simple_json_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/stream/stdio_output_stream.h>
#include <bond/protocol/simple_json_writer.h>

using namespace examples::simple_json;

int main(int argc, char** argv)
{
    const std::string pretty("pretty");
    Widget widget, obj, obj2;

    widget.windows.resize(1);

    // De/serialize from/to a blob
    bond::OutputBuffer buffer;
    bond::SimpleJsonWriter<bond::OutputBuffer> json_writer(buffer);
    bond::Serialize(widget, json_writer);

    bond::SimpleJsonReader<bond::InputBuffer> json_reader(buffer.GetBuffer());
    bond::Deserialize(json_reader, obj);
    BOOST_ASSERT(widget == obj);

    // Deserialize from string
    bond::SimpleJsonReader<const char*> json_string_reader(
         "{\"debug\":\"on\",\"windows\":[{\"title\":\"Sample Konfabulator Widget\",\"width\":500,\"height\":500}]}");
    bond::Deserialize(json_string_reader, obj2);
    BOOST_ASSERT(widget == obj2);

    // Write to standard output
    bond::StdioOutputStream out(stdout);

    if (argc == 1)
    {
        // Default compact
        bond::SimpleJsonWriter<bond::StdioOutputStream> json(out);
        bond::Serialize(widget, json);
    }
    else if(argv[1] == pretty)
    {
        // Pretty format with default 4 space indentation
        bond::SimpleJsonWriter<bond::StdioOutputStream> json(out, true);
        bond::Serialize(widget, json);
    }
    else
    {
        // Custom indentation (0-8 spaces)
        bond::SimpleJsonWriter<bond::StdioOutputStream> json(out, true, atoi(argv[1]));
        bond::Serialize(widget, json);
    }

    return 0;
}
