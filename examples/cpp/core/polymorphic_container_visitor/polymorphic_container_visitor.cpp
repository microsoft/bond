#include "polymorphic_container_visitor_reflection.h"
#include "polymorphic_visitor.h"

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


struct Extract : private boost::noncopyable
{
    Extract(int& n)
        : n(n)
    {}

    template <typename T>
    void operator()(const bond::bonded<T>& bonded) const
    {
        T obj;

        bonded.Deserialize(obj);
        n = obj.n;
    }

    int& n;
};


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
    // a polymorpic list field delcared as vector<bonded<Base>>. Application 
    // can deserialize the Base part of each element and based of the contained 
    // type metadata, deserialize the full element to an appropriate struct.

    Polymorphic polymorphic;

    bonded.Deserialize(polymorphic);
    
    for (size_t i = 0; i < polymorphic.items.size(); ++i)
    {
        int n;
        
        typedef boost::mpl::list<
            Struct1, 
            Struct2
        >::type types;
        
        // Apply the Extract visitor after resolving the element to one of the 
        // listed polymorphic types based on value of the Base::name field. 
        polymorphic::Apply<types, Base::Schema::var::name>
            (Extract(n), polymorphic.items[i]);

        BOOST_VERIFY(n - 1 == static_cast<int>(i));
    }    
        
    return 0;    
}
