#include "merge_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::merge;


int main()
{
    typedef bond::CompactBinaryWriter<bond::OutputBuffer> Writer;
    typedef bond::CompactBinaryReader<bond::InputBuffer>  Reader;

    Struct from;

    from.item.f = 3.14f;
    from.item.str = "original";
    from.items.push_back(from.item);

    bond::OutputBuffer output;
    {
        // Serialize the object
        Writer writer(output);
        Serialize(from, writer);
    }

    // Deserialize payload into a view instance
    StructView view;
    Reader payload(output.GetBuffer());
    Deserialize(payload, view);

    bond::OutputBuffer merged;
    {
        // Modify an element of items container
        view.items[0].str = "merged element";

        Base item;

        view.item.Deserialize(item);
        item.str = "merged bonded";

        // Modify bonded<T> field item by merging changes into it
        view.item.Merge(item);

        // Merge the payload and the modified view instance
        Writer writer(merged);
        Merge(view, payload, writer);
    }
    
    // De-serialize the merged payload
    Struct to;
    Reader reader(merged.GetBuffer());
    Deserialize(reader, to);

    BOOST_ASSERT(to.items[0].f == from.items[0].f);
    BOOST_ASSERT(to.items[0].str == "merged element");
    BOOST_ASSERT(to.item.f == from.item.f);
    BOOST_ASSERT(to.item.str == "merged bonded");

    return 0;    
}
