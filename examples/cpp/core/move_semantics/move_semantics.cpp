#include "move_semantics_reflection.h"

#include <stdlib.h>
#include <algorithm>

using namespace examples::move_semantics;

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
void sink(Struct&& source) 
{
    source.value = 0;
}

Struct create()
{
    Struct result;
    result.value = rand();
    
    for (size_t i = 0; i < 5; i++) 
    {
        Item item;        
        item.name = "name";
        result.items.push_back(item);
    }

    return result;
}

std::vector<Struct> create(size_t count)
{
    std::vector<Struct> result(count);
    
    for (size_t i = 0; i < count; i++) 
    {
        result[i].value = rand();
    }

    return result; 
}
#endif

int main()
{
#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    // no copy constructors invoked below
    Struct s1 = create();

    std::vector<Struct> v1 = create(100);

#ifndef BOND_NO_CXX11_LAMBDAS
    std::sort(std::begin(v1), std::end(v1), 
        [](const Struct& s1, const Struct& s2){ return s1.value < s2.value; });
#endif

    sink(std::move(s1));
#endif

    return 0;
}
