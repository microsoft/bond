#include <boost/type_traits.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/member.hpp>

#include <bond/core/container_interface.h>
#include <bond/core/reflection.h>

namespace bond
{
    // TODO: check that index_specifier_type_list contains sequenced<> and doesn't contain
    // unique indices; otherwise multi_index_container will not be compatible with Bond list.
    template <typename Value, typename IndexSpecifierList, typename Allocator> struct 
    is_list_container<boost::multi_index::multi_index_container<Value, IndexSpecifierList, Allocator> > 
        : boost::true_type {};


    template <typename Value, typename IndexSpecifierList, typename Allocator> struct
    require_modify_element<boost::multi_index::multi_index_container<Value, IndexSpecifierList, Allocator> >
        : boost::true_type {};

    
    template <typename Value, typename IndexSpecifierList, typename Allocator, typename Modify>
    void modify_element(boost::multi_index::multi_index_container<Value, IndexSpecifierList, Allocator>& list, 
                        typename boost::multi_index::multi_index_container<Value, IndexSpecifierList, Allocator>::iterator element, 
                        Modify deserialize)
    {
        list.modify(element, deserialize);
    }


    template <typename Value, typename IndexSpecifierList, typename Allocator>
    class enumerator<boost::multi_index::multi_index_container<Value, IndexSpecifierList, Allocator> >
    {
        typedef boost::multi_index::multi_index_container<Value, IndexSpecifierList, Allocator> List;

    public:
        enumerator(List& list)
            : it(list.begin()),
              end(list.end())
        {}

        bool more() const
        {
            return it != end;
        }

        typename List::iterator 
        next()
        {
            return it++;
        }

    private:
        typename List::iterator it, end;
    };
}

    
// Help to define multi_index::ordered_non_unique index for field in Bond structure
template <typename T>
struct ordered_non_unique_field;
    
template <
    uint16_t id,
    typename modifierTag,
    typename structType,
    typename fieldType,
    fieldType structType::*fieldAddr,
    const bond::Metadata* metadata
>
struct ordered_non_unique_field<
    bond::reflection::FieldTemplate<
        id, 
        modifierTag, 
        structType, 
        fieldType, 
        fieldAddr, 
        metadata
    >
>   : boost::multi_index::ordered_non_unique<
        boost::multi_index::tag<
            bond::reflection::FieldTemplate<id, modifierTag, structType, fieldType, fieldAddr, metadata> 
        >,
        boost::multi_index::member<structType, fieldType, fieldAddr>
        >
{};
