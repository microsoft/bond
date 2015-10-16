#pragma once

#include <bond/core/reflection.h>

#define UT_Equal(x, y) UT_AssertIsTrue(Equal(x, y))

template <typename T1, typename T2>
bool operator==(const SkipStruct<T1>& left, const SkipStruct<T2>& right);

template <typename T>
bool operator==(const SkipStruct<T>& left, const SkipStruct2<T>& right);

template <typename T1, typename T2>
bool operator==(const BondStruct<T1>& left, const BondStruct<T2>& right);

template <typename T1, typename T2>
typename boost::enable_if<bond::is_container<T1>, bool>::type 
Equal(const T1& lhs, const T2& rhs);

template <typename T>
class Compare
{
public:
    Compare(const T& left, const T& right, bool& equal)
        : left(left),
          right(right),
          equal(equal)
    {}

    template <typename Field>
    void operator()(const Field&);

private:
    Compare& operator=(const Compare&);

    const T& left;
    const T& right;
    bool&    equal;
};


inline bool Equal(double left, double right)
{
    // http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
    int64_t l = *(int64_t*)&left;
    if (l < 0)
        l = 0x8000000000000000LL - l;

    int64_t r = *(int64_t*)&right;
    if (r < 0)
        r = 0x8000000000000000LL - r;

    return (l - r) < 5 && (l - r) > -5;
}


inline bool Equal(const bond::blob& lhs, const bond::blob& rhs)
{
    return lhs == rhs;
}


template <typename T>
typename boost::enable_if<bond::has_schema<T>, bool>::type
Equal(const T& left, const T& right)
{
    bool equal = true;
    boost::mpl::for_each<typename T::Schema::fields>(Compare<T>(left, right, equal));
    return equal;
}

// "loose" equality for matching but different types
template <typename T1, typename T2>
typename boost::enable_if<bond::is_basic_type<T1>, bool>::type 
Equal(const T1& lhs, const T2& rhs)
{
    return bond::cast<T2>(lhs) == rhs;
}


// "loose" equality for struct just calls ==
template <typename T1, typename T2>
typename boost::enable_if_c<bond::has_schema<T1>::value 
                         && bond::has_schema<T2>::value, bool>::type 
Equal(const T1& lhs, const T2& rhs)
{
    return lhs == rhs;
}


// "loose" equality for T and bonded<T>
template <typename T1, typename T2>
bool Equal(const T1& lhs, const bond::bonded<T2>& rhs)
{
    T2 rhs_value;

    rhs.Deserialize(rhs_value);

    return Equal(lhs, rhs_value);
}


template <typename T1, typename T2>
bool Equal(const bond::bonded<T1>& lhs, const T2& rhs)
{
    T1 lhs_value;

    lhs.Deserialize(lhs_value);

    return Equal(lhs_value, rhs);
}


template <typename T1, typename T2>
bool Equal(const bond::bonded<T1>& lhs, const bond::bonded<T2>& rhs)
{
    T1 lhs_value;

    lhs.Deserialize(lhs_value);

    T2 rhs_value;

    rhs.Deserialize(rhs_value);

    return Equal(lhs_value, rhs_value);
}


// "loose" equality for pairs
template <typename T1, typename T2, typename T3, typename T4>
bool Equal(const std::pair<T1, T2>& p1, const std::pair<T3, T4>& p2)
{
    return Equal(p1.first, p2.first) && Equal(p1.second, p2.second);
}


template <typename T1, typename T2>
bool Equal(const std::pair<T1, T2>& p1, const std::pair<T1, T2>& p2)
{
    return Equal(p1.first, p2.first) && Equal(p1.second, p2.second);
}


// "loose" equality for lists of matching but different types
template <typename T1, typename T2>
typename boost::enable_if<bond::is_container<T1>, bool>::type 
Equal(const T1& lhs, const T2& rhs)
{
    if (container_size(lhs) != container_size(rhs))
        return false;

    bond::const_enumerator<T1>  lhs_items(lhs);
    bond::const_enumerator<T2>  rhs_items(rhs);

    while (lhs_items.more() && rhs_items.more())
    {
        if (!Equal(lhs_items.next(), rhs_items.next()))
            return false;
    }

    return true;
}


inline bool Equal(const bond::blob& lhs, const std::vector<int8_t>& rhs)
{
    return (lhs.length() == rhs.size() && rhs.size() == 0)
        || !memcmp(lhs.content(), &rhs[0], rhs.size());
}


inline bool Equal(const std::vector<int8_t>& lhs, const bond::blob& rhs)
{
    return (rhs.length() == lhs.size() && lhs.size() == 0)
        || !memcmp(rhs.content(), &lhs[0], lhs.size());
}


template <typename T1, typename T2>
typename boost::enable_if_c<bond::has_base<T1>::value 
                         && bond::has_base<T2>::value, bool>::type
BaseIsEqual(const T1& left, const T2& right)
{
    return Equal(static_cast<const typename T1::Schema::base&>(left),
                 static_cast<const typename T2::Schema::base&>(right));
}


template <typename T1, typename T2>
typename boost::disable_if_c<bond::has_base<T1>::value
                          || bond::has_base<T2>::value, bool>::type
BaseIsEqual(const T1& /*left*/, const T2& /*right*/)
{
    return true;
}


template <typename T1, typename T2>
bool operator==(const BondStruct<T1>& left, const BondStruct<T2>& right)
{
    return Equal(left.field, right.field);
}


template <typename T1, typename T2>
bool operator!=(const BondStruct<T1>& left, const BondStruct<T2>& right)
{
    return !(left == right);
}


inline bool all_empty(const bond::blob& x, bool root = false)
{
    return x.empty() && root;
}

template <typename T>
typename boost::disable_if<bond::is_container<T>, bool>::type
all_empty(const T& x, bool root = false)
{
    return x == T() && root;
}


template <typename T1, typename T2>
inline bool all_empty(const std::pair<T1, T2>& x)
{
    return all_empty(x.second);
}


template <typename T>
typename boost::enable_if<bond::is_container<T>, bool>::type
all_empty(const T& x, bool = false)
{
    if (!x.empty())
        for (bond::const_enumerator<T> items(x); items.more();)
            if (!all_empty(items.next()))
                return false;

    return true;
}


template <typename T1, typename T2>
bool operator==(const SkipStruct<T1>& left, const SkipStruct<T2>& right)
{
    UT_AssertIsTrue(all_empty(right.field1, true));
    return left.field2 == right.field2;
}


template <typename T1, typename T2>
bool operator!=(const SkipStruct<T1>& left, const SkipStruct<T2>& right)
{
    return !(left == right);
}


template <typename T>
bool operator==(const SkipStruct<T>& left, const SkipStruct2<T>& right)
{
    return left.field2 == right.field2;
}


template <typename T>
bool operator!=(const SkipStruct<T>& left, const SkipStruct2<T>& right)
{
    return !(left == right);
}

template <typename T>
template <typename Field>
void Compare<T>::operator()(const Field&)
{
    equal = equal && Equal(Field::GetVariable(left), Field::GetVariable(right));
}

