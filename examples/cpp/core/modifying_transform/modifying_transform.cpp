#include "modifying_transform_reflection.h"
#include <bond/core/bond.h>

using namespace examples::modifying_transform;

class Setter
    : public bond::ModifyingTransform
{
public:
    void Begin(const bond::Metadata& metadata) const
    {
        _struct = metadata.name;
    }

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(T& base) const
    {
        return Apply(Setter(), base);
    }

    // Struct fields
    template <typename T>
    typename boost::enable_if<bond::is_bond_type<T>, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& /*metadata*/, T& field) const
    {
        return Apply(Setter(), field);
    }

    // Other fields
    template <typename T>
    typename boost::disable_if<bond::is_bond_type<T>, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& metadata, T& field) const
    {
        // This simple example assumes that all non-struct fields are strings
        field = _struct + "::" + metadata.name;
        return false;
    }

private:
    mutable std::string _struct;
};


int main()
{
    Struct obj;

    // Set values of all fields to their names
    Apply(Setter(), obj);
    
    return 0;    
}
