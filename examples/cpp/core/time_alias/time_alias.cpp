#include "time_alias_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::time;

int main()
{
    Example obj, obj2;

    using examples::time::time;
    using namespace boost::gregorian;
    using namespace boost::posix_time;

    // In the generated code we use boost::posix_time_ptime::ptime to represent
    // the type alias 'time' (see makefile.inc for code gen flags).
    obj.when = time(date(2016, 1, 29));
    obj.bdays["bill"] = time(from_string("1955/10/28"));

    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    // Serialize the object
    Serialize(obj, writer);

    bond::CompactBinaryReader<bond::InputBuffer> reader(output.GetBuffer());

    // De-serialize the object
    Deserialize(reader, obj2);

    std::string followingSunday = to_simple_string(
        first_day_of_the_week_after(Sunday).get_date(obj2.when.date()));

    BOOST_ASSERT(obj == obj2);

    return 0;
}

