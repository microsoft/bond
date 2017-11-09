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


struct Extract
{
    explicit Extract(int& n)
        : n(n)
    {}

    Extract(const Extract&) = delete;
    Extract& operator=(const Extract&) = delete;

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
    // a polymorpic list field delcared as vector<bonded<Base>>. Application
    // can deserialize the Base part of each element and based of the contained
    // type metadata, deserialize the full element to an appropriate struct.

    Polymorphic polymorphic;
    bonded.Deserialize(polymorphic);

    size_t count = 0;
    for (const auto& item : polymorphic.items)
    {
        typedef boost::mpl::map<
            boost::mpl::pair<std::integral_constant<StructKind, StructKind_Struct1>, Struct1>,
            boost::mpl::pair<std::integral_constant<StructKind, StructKind_Struct2>, Struct2>
        > types;


        int n;
        // Apply the Extract visitor after resolving the element to one of the
        // listed polymorphic types based on value of the Base::kind field.
        polymorphic::Apply<types, Base::Schema::var::kind>(
            Extract(n),
            item);

        BOOST_VERIFY(n - 1 == static_cast<int>(count));
        ++count;
    }

    return 0;
}
