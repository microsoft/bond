#pragma once 

#include "compat_types.h"
#include "compat_no_generics_types.h"
#include "cmd_arg_types.h"
#include <bond/core/bond.h>

using namespace unittest::compat;

void Init(Test test, Compat& obj);
bond::blob Serialize(Test test, const Compat& obj);
void Deserialize(Test test, const bond::blob& buffer, Compat& obj, bond::SchemaDef& schema);
void Convert(const Compat& from, CompatNoGenerics& to);

const int MAX_SIZE = 6 * 1024 * 1024;
