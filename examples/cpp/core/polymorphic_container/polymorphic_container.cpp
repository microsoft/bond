#include "polymorphic_container_reflection.h"

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


void DeserializeStruct1(const bond::bonded<Base>& base)
{
    // Explicit down-casting from bonded<Base> to bonded<Struct1>
    bond::bonded<Struct1> bonded(base);       

    Struct1 obj;

    bonded.Deserialize(obj);
}

                                                                        
void DeserializeStruct2(const bond::bonded<Base>& base)
{
    // Explicit down-casting from bonded<Base> to bonded<Struct2>
    bond::bonded<Struct2> bonded(base);

    Struct2 obj;

    bonded.Deserialize(obj);
}


int main()
{
    bond::bonded<Polymorphic> bonded;
    
    {
        Struct1 obj1;

        obj1.n = 1;
        obj1.str = "1";

        Struct2 obj2;

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
    
    for (std::list<bond::bonded<Base> >::const_iterator it = polymorphic.items.begin(); it != polymorphic.items.end(); ++it)
    {
        Base base;

        it->Deserialize(base);

        // The field Base::name is declared as bond_meta::full_name, and thus
        // was automatically initialized by the system to fully qualified
        // name of the struct.
        if (base.name == Struct1::Schema::metadata.qualified_name)
        {
            DeserializeStruct1(*it);
        }
        else if (base.name == Struct2::Schema::metadata.qualified_name)
        {
            DeserializeStruct2(*it);
        }
    }
    
    return 0;    
}
