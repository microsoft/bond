#include "precompiled.h"
#include "apply_test_reflection.h"

void Init(unittest::apply::Struct& obj)
{
    obj = InitRandom<unittest::apply::Struct>();
}


void Init(unittest::apply::Derived& obj)
{
    obj = InitRandom<unittest::apply::Derived>();
}
        