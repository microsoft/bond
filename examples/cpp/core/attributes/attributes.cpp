#include "attributes_reflection.h"
#include <bond/core/bond.h>
#include <boost/lexical_cast.hpp>

using namespace examples::attributes;

class Validator
    : public bond::SerializingTransform
{
public:
    void Begin(const bond::Metadata&) const
    {}

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(const T& value) const
    {
        // Since Validator is stateless we use the same instance to validate the base
        bond::Apply(*this, value);
        return false;
    }

    template <typename T>
    typename boost::enable_if<std::is_integral<T>, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& metadata, const T& value) const
    {
        // If Min and/or Max attribute is defined for the field, we check if
        // the field value is within the specified range.
        std::map<std::string, std::string>::const_iterator it;

        it = metadata.attributes.find("Min");

        if (it != metadata.attributes.end())
        {
            if (value < boost::lexical_cast<T>(it->second))
            {
                BOND_THROW(bond::CoreException,
                    "Invalid value " << value << " for field " << metadata.name <<
                    " is lower than allowed minimum of " << it->second << ".");
            }
        }

        it = metadata.attributes.find("Max");

        if (it != metadata.attributes.end())
        {
            if (value > boost::lexical_cast<T>(it->second))
            {
                BOND_THROW(bond::CoreException,
                    "Invalid value " << value << " for field " << metadata.name <<
                    " is higher than allowed maximum of " << it->second << ".");
            }
        }

        return false;
    }

    template <typename T>
    typename boost::disable_if_c<std::is_integral<T>::value
                              || bond::is_bond_type<T>::value, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& /*metadata*/, const T& /*value*/) const
    {
        return false;
    }

    // struct
    template <typename T>
    typename boost::enable_if<bond::is_bond_type<T>, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& /*metadata*/, const T& value) const
    {
        // Since Validator is stateless we use the same instance to validate nested objects
        bond::Apply(*this, value);
        return false;
    }
};


int main()
{
    Struct obj;

    // values out of range
    obj.s.n = 4;
    obj.s.f = 4;

    try
    {
        bond::Apply(Validator(), obj);
    }
    catch(const bond::Exception&)
    {
        // values in range
        obj.s.n = 13;
        obj.s.f = -3;

        bond::Apply(Validator(), obj);
    }

    return 0;
}
