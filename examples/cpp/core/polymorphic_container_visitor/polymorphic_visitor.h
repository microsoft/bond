#pragma once

#include <bond/core/bond.h>

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

        template <typename U>
        void operator()(const U*) const
        {
            if (U::Schema::metadata.qualified_name == Field::GetVariable(_base))
                _visitor(bond::bonded<U>(_polymorphic));
        }

    private:
        Resolver& operator=(const Resolver&);

        const T&                _base;
        const bond::bonded<T>&  _polymorphic;
        const Visitor&          _visitor;
    };

} // namespace detail




// Generic function which resolves a polymorphic object to one of the specified
// Types using a bond_meta field Field and calls visitor with bonded<T> casted 
// to the resolved type.
template <typename Types, typename Field, typename T, typename Visitor>
void Apply(const Visitor& visitor, const bond::bonded<T>& polymorphic)
{
    using boost::mpl::_;

    T base;
    detail::Resolver<Field, T, Visitor> resolver(base, polymorphic, visitor);

    typedef typename boost::mpl::transform<Types, boost::add_pointer<_> >::type TypePtr;

    polymorphic.Deserialize(base);
    boost::mpl::for_each<TypePtr>(resolver);
}
 

} // namespace polymorphic
