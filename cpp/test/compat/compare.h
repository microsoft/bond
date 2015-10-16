#pragma once

template <typename T>
class Compare;

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


template <typename T>
typename boost::enable_if<bond::is_basic_type<T>, bool>::type
Equal(const T& left, const T& right)
{
    return left == right;
}


inline bool Equal(const bond::blob& left, const bond::blob& right)
{
    return left == right;
}


template <typename T>
typename boost::enable_if<bond::has_schema<T>, bool>::type
Equal(const T& left, const T& right)
{
    bool equal = true;
    boost::mpl::for_each<typename T::Schema::fields>(Compare<T>(left, right, equal));
    return equal;
}


template <typename T>
inline bool Equal(const bond::bonded<T>& left, const bond::bonded<T>& right)
{
    T left_value, right_value;

    left.Deserialize(left_value);
    right.Deserialize(right_value);

    return Equal(left_value, right_value);
}


template <typename T1, typename T2>
bool Equal(const std::pair<T1, T2>& left, const std::pair<T1, T2>& right);


template <typename T>
typename boost::enable_if<bond::is_container<T>, bool>::type
Equal(const T& left, const T& right)
{
    bond::const_enumerator<T> left_items(left), right_items(right);

    while (left_items.more() && right_items.more())
        if (!Equal(left_items.next(), right_items.next()))
            return false;

    return !left_items.more() && !right_items.more();
}


template <typename T1, typename T2>
bool Equal(const std::pair<T1, T2>& left, const std::pair<T1, T2>& right)
{
    return Equal(left.first, right.first) 
        && Equal(left.second, right.second);
}


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
    void operator()(const Field&)
    {
        equal = equal && Equal(Field::GetVariable(left), Field::GetVariable(right));
    }

private:
    Compare& operator=(const Compare&);

    const T& left;
    const T& right;
    bool&    equal;
};
