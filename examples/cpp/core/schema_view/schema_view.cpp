#include "schema_view_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::schema_view;


int main()
{
    Example example;
    
    example.num = 42;
    example.str = "test";
    example.items.push_back(3.14);
    example.items.push_back(0);
    
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    Marshal(example, writer);

    ExampleView view;

    bond::InputBuffer input(output.GetBuffer());

    Unmarshal(input, view);
    
    BOOST_ASSERT(example.num == view.num);
    BOOST_ASSERT(example.str == view.str);
    
    return 0;    
}
