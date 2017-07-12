#pragma once

#include <bond/core/reflection.h>

#include <algorithm>
#include <complex>
#include <limits>

#define UT_Equal_P(x, y, Protocols) UT_AssertIsTrue(Equal<Protocols>(x, y))
#define UT_Equal(x, y) UT_Equal_P(x, y, bond::BuiltInProtocols)
#define UT_Compare_P(x, y, Protocols) UT_AssertIsTrue(Compare<Protocols>(x, y))
#define UT_Compare(x, y) UT_Compare_P(x, y, bond::BuiltInProtocols)
#define UT_NegateCompare_P(x, y, Protocols) UT_AssertIsTrue(!Compare<Protocols>(x, y))
#define UT_NegateCompare(x, y) UT_NegateCompare_P(x, y, bond::BuiltInProtocols)

template <typename Protocols, typename T1, typename T2>
bool Compare(const SkipStruct<T1>& left, const SkipStruct<T2>& right);

template <typename Protocols, typename T>
bool Compare(const SkipStruct<T>& left, const SkipStruct2<T>& right);

template <typename Protocols, typename T1, typename T2>
bool Compare(const BondStruct<T1>& left, const BondStruct<T2>& right);

template <typename Protocols, typename NestedStruct1>
bool Compare(const NestedStruct1& left, const NestedStruct1OptionalBondedView& right);

template <typename Protocols>
bool Compare(const NestedStruct1& left, const NestedStruct1BondedView& right);

template <typename Protocols>
bool Compare(const SimpleStruct& left, const SimpleStructView& right);

template <typename Protocols>
bool Compare(const NestedStruct& left, const NestedStructView& right);

template <typename Protocols, typename T>
bool Compare(const WithSimpleList<T>& custom, const BondStruct<std::list<T> >& standard);

template <typename Protocols, typename T>
bool Compare(const BondStruct<std::list<T> >& standard, const WithSimpleList<T>& custom);

template <typename Protocols>
bool Compare(const WithStaticString& custom, const BondStruct<std::string>& standard);

template <typename Protocols>
bool Compare(const BondStruct<std::string>& standard, const WithStaticString& custom);

template <typename Protocols>
bool Compare(const WithStaticWString& custom, const BondStruct<std::wstring>& standard);

template <typename Protocols>
bool Compare(const BondStruct<std::wstring>& standard, const WithStaticWString& custom);

template <typename Protocols>
bool Compare(const StructWithBase& left, const StructWithBaseView& right);

template <typename Protocols>
bool Compare(const SimpleBase& left, const SimpleBaseView& right);

template <typename Protocols>
bool Compare(const NestedWithBase& left, const NestedWithBaseView& right);

template <typename Protocols>
bool Compare(const ListWithBase& left, const ListWithBaseView& right);

template <typename Protocols>
bool Compare(const StructWithBase& left, const SimpleStruct& right);

template <typename Protocols>
bool Compare(const StructWithBase& left, const SimpleBase& right);

template <typename Protocols>
bool Compare(const ListWithBase& left, const ListOfBase& right);

template <typename Protocols>
bool Compare(const NestedWithBase& left, const NestedWithBase2& right);

template <typename Protocols>
bool Compare(const SimpleListsStruct& left, const SimpleListsStructView& right);

template <typename Protocols>
bool Compare(const NestedListsStruct& left, const NestedListsStructView& right);

template <typename Protocols>
bool Compare(const Required& left, const RequiredViewGood& right);

template <typename Protocols, typename T1, typename T2>
typename boost::enable_if<bond::is_container<T1>, bool>::type
Equal(const T1& lhs, const T2& rhs);

template <typename T, typename Protocols>
class Comparer
{
public:
    Comparer(const T& left, const T& right, bool& equal)
        : left(left),
          right(right),
          equal(equal)
    {}

    template <typename Field>
    void operator()(const Field&);

private:
    Comparer& operator=(const Comparer&);

    const T& left;
    const T& right;
    bool&    equal;
};


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Compare(const T1& lhs, const T2& rhs)
{
    return lhs == rhs;
}


template <typename Protocols = bond::BuiltInProtocols, typename T>
inline typename boost::enable_if<std::is_floating_point<T>, bool>::type
Equal(T left, T right)
{
    const int ulp = 5;
    return std::abs(left - right) <= std::numeric_limits<T>::epsilon() * (std::max)(std::abs(left), std::abs(right)) * ulp;
}


template <typename Protocols = bond::BuiltInProtocols>
inline bool Equal(const bond::blob& lhs, const bond::blob& rhs)
{
    return Compare<Protocols>(lhs, rhs);
}


template <typename Protocols = bond::BuiltInProtocols, typename T>
typename boost::enable_if<bond::has_schema<T>, bool>::type
Equal(const T& left, const T& right)
{
    bool equal = true;
    boost::mpl::for_each<typename T::Schema::fields>(Comparer<T, Protocols>(left, right, equal));
    return equal;
}

// "loose" equality for matching but different types
template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
typename boost::enable_if<bond::is_basic_type<T1>, bool>::type
Equal(const T1& lhs, const T2& rhs)
{
    return Compare<Protocols>(bond::cast<T2>(lhs), rhs);
}


// "loose" equality for struct just calls Compare
template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
typename boost::enable_if_c<bond::has_schema<T1>::value
                         && bond::has_schema<T2>::value, bool>::type
Equal(const T1& lhs, const T2& rhs)
{
    return Compare<Protocols>(lhs, rhs);
}


// "loose" equality for T and bonded<T>
template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Equal(const T1& lhs, const bond::bonded<T2>& rhs)
{
    T2 rhs_value;

    rhs.template Deserialize<Protocols>(rhs_value);

    return Equal<Protocols>(lhs, rhs_value);
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Equal(const bond::bonded<T1>& lhs, const T2& rhs)
{
    T1 lhs_value;

    lhs.template Deserialize<Protocols>(lhs_value);

    return Equal<Protocols>(lhs_value, rhs);
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Equal(const bond::bonded<T1>& lhs, const bond::bonded<T2>& rhs)
{
    T1 lhs_value;

    lhs.template Deserialize<Protocols>(lhs_value);

    T2 rhs_value;

    rhs.template Deserialize<Protocols>(rhs_value);

    return Equal<Protocols>(lhs_value, rhs_value);
}


// "loose" equality for pairs
template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2, typename T3, typename T4>
bool Equal(const std::pair<T1, T2>& p1, const std::pair<T3, T4>& p2)
{
    return Equal<Protocols>(p1.first, p2.first) && Equal<Protocols>(p1.second, p2.second);
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Equal(const std::pair<T1, T2>& p1, const std::pair<T1, T2>& p2)
{
    return Equal<Protocols>(p1.first, p2.first) && Equal<Protocols>(p1.second, p2.second);
}


// "loose" equality for lists of matching but different types
template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
typename boost::enable_if<bond::is_container<T1>, bool>::type
Equal(const T1& lhs, const T2& rhs)
{
    if (container_size(lhs) != container_size(rhs))
        return false;

    bond::const_enumerator<T1>  lhs_items(lhs);
    bond::const_enumerator<T2>  rhs_items(rhs);

    while (lhs_items.more() && rhs_items.more())
    {
        if (!Equal<Protocols>(lhs_items.next(), rhs_items.next()))
            return false;
    }

    return true;
}


template <typename Protocols = bond::BuiltInProtocols>
inline bool Equal(const bond::blob& lhs, const std::vector<int8_t>& rhs)
{
    return (lhs.length() == rhs.size() && rhs.size() == 0)
        || !memcmp(lhs.content(), &rhs[0], rhs.size());
}


template <typename Protocols = bond::BuiltInProtocols>
inline bool Equal(const std::vector<int8_t>& lhs, const bond::blob& rhs)
{
    return (rhs.length() == lhs.size() && lhs.size() == 0)
        || !memcmp(rhs.content(), &lhs[0], lhs.size());
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
typename boost::enable_if_c<bond::has_base<T1>::value
                         && bond::has_base<T2>::value, bool>::type
BaseIsEqual(const T1& left, const T2& right)
{
    return Equal<Protocols>(static_cast<const typename T1::Schema::base&>(left),
                            static_cast<const typename T2::Schema::base&>(right));
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
typename boost::disable_if_c<bond::has_base<T1>::value
                          || bond::has_base<T2>::value, bool>::type
BaseIsEqual(const T1& /*left*/, const T2& /*right*/)
{
    return true;
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Compare(const BondStruct<T1>& left, const BondStruct<T2>& right)
{
    return Equal<Protocols>(left.field, right.field);
}


template <typename Protocols>
inline bool all_empty(const bond::blob& x, bool root = false)
{
    return x.empty() && root;
}

template <typename Protocols, typename T>
typename boost::disable_if<bond::is_container<T>, bool>::type
all_empty(const T& x, bool root = false)
{
    return Compare<Protocols>(x, T()) && root;
}


template <typename Protocols, typename T1, typename T2>
inline bool all_empty(const std::pair<T1, T2>& x)
{
    return all_empty<Protocols>(x.second);
}


template <typename Protocols, typename T>
typename boost::enable_if<bond::is_container<T>, bool>::type
all_empty(const T& x, bool = false)
{
    if (!x.empty())
        for (bond::const_enumerator<T> items(x); items.more();)
            if (!all_empty<Protocols>(items.next()))
                return false;

    return true;
}


template <typename Protocols = bond::BuiltInProtocols, typename T1, typename T2>
bool Compare(const SkipStruct<T1>& left, const SkipStruct<T2>& right)
{
    UT_AssertIsTrue(all_empty<Protocols>(right.field1, true));
    return Compare<Protocols>(left.field2, right.field2);
}


template <typename Protocols = bond::BuiltInProtocols, typename T>
bool Compare(const SkipStruct<T>& left, const SkipStruct2<T>& right)
{
    return Equal<Protocols>(left.field2, right.field2);
}


template <typename T, typename Protocols>
template <typename Field>
void Comparer<T, Protocols>::operator()(const Field&)
{
    equal = equal && Equal<Protocols>(Field::GetVariable(left), Field::GetVariable(right));
}
