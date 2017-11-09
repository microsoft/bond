#include "string_ref_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/protocol/simple_json_writer.h>

using namespace examples::string_ref;

int main()
{
    Example obj, obj2;

    const char* text =
      "Alice was beginning to get very tired of sitting by her sister on the "
      "bank and of having nothing to do: once or twice she had peeped into the "
      "book her sister was reading but it had no pictures or conversations in "
      "it 'and what is the use of a book' thought Alice 'without pictures or "
      "conversation?'";

    using examples::string_ref::string_ref;

    // Create a vector of string_refs, each pointing to a word in the source
    // text. Using boost::string_ref instead of std::string avoid allocating
    // and coping memory.
    // Note that string_ref can be only used as a read-only string, so we could
    // not deserialize into a string_ref.
    for (const char *begin = text, *end = text;; ++end)
    {
        if (*end == ' ' || *end == '\x0')
        {
            obj.words.push_back(string_ref(begin, end - begin));
            begin = end + 1;
            if (*end == '\x0')
                break;
        }
    }

    // Serialize the object to Json
    bond::OutputBuffer output;
    bond::SimpleJsonWriter<bond::OutputBuffer> writer(output);
    Serialize(obj, writer);

    return 0;
}
