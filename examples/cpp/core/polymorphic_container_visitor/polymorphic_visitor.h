#pragma once

#include <bond/core/bond.h>

#include <boost/mpl/map.hpp>
#include <type_traits>

namespace polymorphic
{

namespace detail
{

    template <typename Field, typename T, typename Visitor>
    class Resolver
    {
    public:
        Resolver(const T& base, const bond::bonded<T>& polymorphic, const Visitor& visitor)
            : _base(base),
              _polymorphic(polymorphic),
              _visitor(visitor)
        {}

        Resolver& operator=(const Resolver&) = delete;

        template <typename Pair>
        void operator()(Pair) const
        {
            if (Pair::first::value == Field::GetVariable(_base))
                _visitor(bond::bonded<typename Pair::second>(_polymorphic));
        }

    private:
        const T&                _base;
        const bond::bonded<T>&  _polymorphic;
        const Visitor&          _visitor;
    };

} // namespace detail




// Generic function which resolves a polymorphic object to one of the
// specified types in TypeMap using an enum Field and calls visitor with
// bonded<T> casted to the resolved type.
//
// TypeMap is expected to be a Boost MPL map of
// std::integral_constant<Field::FieldType> to Type.
template <typename TypeMap, typename Field, typename Visitor, typename Base>
void Apply(const Visitor& visitor, const bond::bonded<Base>& polymorphic)
{
    Base base;
    detail::Resolver<Field, Base, Visitor> resolver(base, polymorphic, visitor);

    polymorphic.Deserialize(base);
    boost::mpl::for_each<TypeMap>(resolver);
}


} // namespace polymorphic
