#pragma once

#include <boost/multi_index_container.hpp>
#include <bond/core/reflection.h>

template <typename T>
class move_check
{
public:
    move_check(const T& src, bool& mov)
        : src(src),
          mov(mov)
    {}

    template <typename Field>
    void operator()(const Field&);

private:
    move_check& operator=(const move_check&);

    const T& src;
    bool& mov;
};

template <typename T>
typename boost::enable_if<bond::has_schema<T>, bool>::type
moved(const T& src)
{
    bool mov = true;
    boost::mpl::for_each<typename T::Schema::fields>(move_check<T>(src, mov));
    return mov;
}


template <typename T>
typename boost::enable_if_c<bond::is_basic_type<T>::value && !bond::is_string_type<T>::value, bool>::type 
moved(const T&)
{
    return true;
}

template <typename T>
typename boost::enable_if<bond::is_string_type<T>, bool>::type 
moved(const T& src)
{
    return string_length(src) == 0;
}

template <typename T>
bool moved(const bond::bonded<T>& src)
{
    return src == bond::bonded<T>();
}

template <typename T>
typename boost::enable_if<bond::is_container<T>, bool>::type 
moved(const T& src)
{
    return container_size(src) == 0;
}

template <typename T, size_t N>
bool moved(const std::array<T, N>&)
{
    return true;
}

template <typename T, typename I>
bool moved(const boost::multi_index::multi_index_container<T, I>&)
{
    return true;
}

template <typename T>
template <typename Field>
void move_check<T>::operator()(const Field&)
{
    mov = mov && moved(Field::GetVariable(src));
}

