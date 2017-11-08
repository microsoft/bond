#include <list>
#include <memory>
#include <bond/core/container_interface.h>

namespace bond
{
    // In order to "tell" Bond how to de/serialize container of shared_ptr<T> we
    // specialize 4 container concepts defined in bond\core\container_interface.h
    // - make_element
    // - element_type
    // - const_enumerator
    // - enumerator
    template <typename T, typename A, template <typename T1, typename A1> class Container>
    inline
    std::shared_ptr<T>
    make_element(Container<std::shared_ptr<T>, A>&)
    {
        return std::shared_ptr<T>();
    }

    
    // element_type
    template <typename T, typename A, template <typename T1, typename A1> class Container> struct 
    element_type<Container<std::shared_ptr<T>, A> >
    {
        // Since Bond doesn't understand shared_ptr, we present this as a container of T
        typedef T type;
    };

    
    // const_enumerator
    template <typename T, typename A, template <typename T1, typename A1> class Container>
    class const_enumerator<Container<std::shared_ptr<T>, A> >
    {
        typedef Container<std::shared_ptr<T>, A> List;

    public:
        const_enumerator(const List& list)
            : it(list.begin()),
              end(list.end())
        {}

        bool more() const 
        {
            return it != end;
        }

        const T& next()
        {
            // Dereference the shared_ptr to return to Bond reference to the object
            return **(it++);
        }

    private:
        typename List::const_iterator it, end;
    };


    // enumerator
    template <typename T, typename A, template <typename T1, typename A1> class Container>
    class enumerator<Container<std::shared_ptr<T>, A> >
    {
        typedef Container<std::shared_ptr<T>, A> List;

    public:
        enumerator(List& list)
            : alloc(list.get_allocator()),
              it(list.begin()),
              end(list.end())
        {}

        bool more() const
        {
            return it != end;
        }

        T& next()
        {
            // If the shared_ptr is NULL, initialize it with a new instance of T.
            if (!*it)
                *it = new_value(alloc);

            // Dereference the shared_ptr to return reference to the object.
            return **(it++);
        }

    private:
        std::shared_ptr<T> 
        new_value(const std::allocator<std::shared_ptr<T> >&) const
        {
            return std::make_shared<T>();
        }

        template <typename Alloc>
        std::shared_ptr<T> 
        new_value(const Alloc& alloc) const
        {
            return std::allocate_shared<T>(alloc, &alloc);
        }

        A alloc;
        typename List::iterator it, end;
    };
}

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include "container_of_pointers_reflection.h"

using namespace examples::container_of_pointers;

int main()
{
    Struct obj;
    
    obj.items.push_back(std::make_shared<Item>());
    obj.items.back()->foo = "A";
    obj.items.back()->bar = 0;
    
    obj.items.push_back(std::make_shared<Item>());
    obj.items.back()->foo = "B";
    obj.items.back()->bar = 1;

    bond::OutputBuffer buffer;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);

    // Serialize list of std::shared_ptr<Item>
    Serialize(obj, writer);

    // Deserialize as list of std::shared_ptr<Item>
    Struct obj2;

    bond::CompactBinaryReader<bond::InputBuffer> reader(buffer.GetBuffer());
    
    Deserialize(reader, obj2);

    assert(*obj2.items.front() == *obj.items.front());
    assert(*obj2.items.back() == *obj.items.back());
    
    return 0;    
}
