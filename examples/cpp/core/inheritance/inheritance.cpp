#include "inheritance_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::inheritance;

template <typename T>
bond::blob Serialize(const T& obj)
{
    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
    bond::Serialize(obj, writer);

    return buffer.GetBuffer();
}


int main()
{
    Base base;

    base.str = "base";
    bond::CompactBinaryReader<bond::InputBuffer> base_payload(Serialize(base));

    Derived derived;
    
    derived.str = "derived";
    derived.Base::str = "derived base";
    bond::CompactBinaryReader<bond::InputBuffer> derived_payload(Serialize(derived));

    {
        Base obj;
    
        // Deserialize Base from payload containing Derived
        // This is equivalent to C++ construct:
        //      obj = derived;
        bond::Deserialize(derived_payload, obj);

        assert(obj == static_cast<const Base&>(derived));
    }

    {
        Derived obj;
    
        // Deserialize Base part of Derived from payload containing Base
        // This is equivalent to C++ construct:
        //      static_cast<Base&>(obj) = base;
        bond::Deserialize(base_payload, static_cast<Base&>(obj));

        assert(static_cast<const Base&>(obj) == base);
    }
    
    return 0;
}
