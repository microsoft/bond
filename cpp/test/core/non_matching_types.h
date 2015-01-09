#pragma once 

using namespace std;


template <typename T, typename Enable = void> struct 
non_matching_types;


// exmaples of non-matching types for a basic type T: BondStruct<T> or non-matching sign-ness
template <typename T> struct 
non_matching_types<T, typename boost::enable_if<bond::is_basic_type<T> >::type>
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        bond::blob,
#endif
        typename boost::mpl::if_<std::is_unsigned<T>, int64_t, BondStruct<T> >::type
    > type;
};


// examples of non-matching types for list<T>: vector<list<T> >, set<T>, T and BondStruct<T> 
template <typename T> struct 
non_matching_types<T, typename boost::enable_if<bond::is_list_container<T> >::type>
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        typename boost::mpl::if_<bond::is_basic_type<typename bond::element_type<T>::type>, 
            set<typename bond::element_type<T>::type>,
            vector<T> >::type,
        typename bond::element_type<T>::type, 
        bond::blob,
#endif
        BondStruct<typename bond::element_type<T>::type>
    > type;
};


// examples of non-matching types for set<T>: list<set<T> >, T, BondStruct<T> 
template <typename T> struct 
non_matching_types<T, typename boost::enable_if<bond::is_set_container<T> >::type>
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        list<T>,
        bond::blob,
        typename bond::element_type<T>::type, 
#endif
        BondStruct<typename bond::element_type<T>::type>
    > type;
};


// examples of non-matching types for map<K, T>: list<map<K,T> >, BondStruct<T> 
template <typename T> struct 
non_matching_types<T, typename boost::enable_if<bond::is_map_container<T> >::type>
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        list<T>,
        bond::blob,
#endif
        BondStruct<typename bond::element_type<T>::type::second_type>
    > type;
};


// examples of non-matching types for a Bond struct: string and list<bool>
template <typename T> struct 
non_matching_types<T, typename boost::enable_if<bond::is_bond_type<T> >::type>
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        string, 
        bond::blob,
#endif
        list<bool>
    > type;
};


// examples of non-matching types for bond::blob: int8_t and list<uint8_t>
template <> struct 
non_matching_types<bond::blob>
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        int8_t, 
#endif
        list<uint8_t>
    > type;
};

