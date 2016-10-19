#include "polymorphic_container_reflection.h"

#include <cassert>
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::polymorphic_containers;


template <typename T>
bond::bonded<T> Serialize(const T& obj)
{
    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
    bond::Serialize(obj, writer);

    bond::CompactBinaryReader<bond::InputBuffer> reader(buffer.GetBuffer());
    return bond::bonded<T>(reader);
}


Struct1 DeserializeStruct1(const bond::bonded<Base>& base)
{
    // Explicit down-casting from bonded<Base> to bonded<Struct1>
    bond::bonded<Struct1> bonded(base);
    Struct1 obj;
    bonded.Deserialize(obj);
    return obj;
}


Struct2 DeserializeStruct2(const bond::bonded<Base>& base)
{
    // Explicit down-casting from bonded<Base> to bonded<Struct2>
    bond::bonded<Struct2> bonded(base);
    Struct2 obj;
    bonded.Deserialize(obj);
    return obj;
}


int main()
{
    bond::bonded<Polymorphic> bonded;

    {
        Struct1 obj1;
        obj1.kind = StructKind_Struct1;
        obj1.n = 1;
        obj1.str = "1";

        Struct2 obj2;
        obj2.kind = StructKind_Struct2;
        obj2.n = 2;
        obj2.str = "2";

        Polymorphic polymorphic;

        // Implicit up-casting from bonded<Struct1/2> to bonded<Base>
        polymorphic.items.push_back(Serialize(obj1));
        polymorphic.items.push_back(Serialize(obj2));

        bonded = Serialize(polymorphic);
    }

    // At this point bonded represents serialized data for a struct which has
    // a polymorpic list field delcared as list<bonded<Base>>. Application can
    // deserialize the Base part of each element and based of the contained type
    // metadata, deserialize the full element to an appropriate struct.

    Polymorphic polymorphic;

    bonded.Deserialize(polymorphic);

    for (
        std::list<bond::bonded<Base> >::const_iterator it = polymorphic.items.begin();
        it != polymorphic.items.end();
        ++it)
    {
        Base base;
        it->Deserialize(base);

        switch (base.kind)
        {
            case StructKind_Struct1:
            {
                Struct1 obj1 = DeserializeStruct1(*it);
                assert(obj1.str == "1");
                assert(obj1.n == 1);
            }
            break;

            case StructKind_Struct2:
            {
                Struct2 obj2 = DeserializeStruct2(*it);
                assert(obj2.b == false);
                assert(obj2.n == 2);
                assert(obj2.str == "2");
            }
            break;

            default:
                assert(false);
                break;
        }
    }

    return 0;
}
