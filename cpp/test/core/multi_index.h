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


template <typename T> struct
get_field_template
{
    template <uint16_t field_id, typename ModifierTag, typename Struct, typename FieldType, FieldType Struct::*field_ptr, const bond::Metadata* metadata_ptr>
    static bond::reflection::FieldTemplate<field_id, ModifierTag, Struct, FieldType, field_ptr, metadata_ptr> helper(
        const bond::reflection::FieldTemplate<field_id, ModifierTag, Struct, FieldType, field_ptr, metadata_ptr>&);

    typedef decltype(helper(T())) type;
};


template <typename T, typename U = typename get_field_template<T>::type>
struct ordered_non_unique_field;


template <typename T, uint16_t field_id, typename ModifierTag, typename Struct, typename FieldType, FieldType Struct::*field_ptr, const bond::Metadata* metadata_ptr>
struct ordered_non_unique_field<T, bond::reflection::FieldTemplate<field_id, ModifierTag, Struct, FieldType, field_ptr, metadata_ptr> >
    : boost::multi_index::ordered_non_unique<
        boost::multi_index::tag<
            T
        >,
        boost::multi_index::member<Struct, FieldType, field_ptr>
        >
{};
