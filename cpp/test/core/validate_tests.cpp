#include "precompiled.h"
#include "validate_tests.h"
#include <bond/core/validate.h>
#include <validation_reflection.h>

using namespace unit_tests::validation;

enum Result
{
    Error, 
    False,
    True
};

template<typename T1, typename T2> 
inline Result Check(const T1& src, const T2& dst)
{
    try 
    {
        return bond::Validate(src, dst) ? True : False;
    }
    catch (const bond::SchemaValidateException&)
    {
        return Error;
    }
}

template<typename T1, typename T2> 
inline Result CheckTwoWay(const T1& s1, const T2& s2)
{
    try 
    {
        return bond::ValidateTwoWay(s1, s2) ? True : False;
    }
    catch (const bond::SchemaValidateException&)
    {
        return Error;
    }
}


template <typename T>
bond::CompactBinaryReader<bond::InputBuffer> Serialize(const T& obj)
{
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    Serialize(obj, writer);

    return bond::InputBuffer(output.GetBuffer());
}


bond::CompactBinaryReader<bond::InputBuffer> 
Incompatible(const bond::SchemaDef& schema)
{
    IncompatibleSchemaDef incompatible;

    incompatible.root = schema.root;
    
    for (auto it = schema.structs.begin(); it != schema.structs.end(); ++it)
    {
        IncompatibleStructDef structDef;

        structDef.metadata = it->metadata;
        structDef.base_def = it->base_def;
        structDef.fields = it->fields;

        incompatible.structs.push_back(structDef);
    }

    incompatible.structs.back().extra_field = true;
    
    return Serialize(incompatible);
}


template<typename T1, typename T2> 
inline bool Check(Result forward,
                  Result reverse,
                  Result bidirectional)
{
    bool result = true;
    
    bond::RuntimeSchema s1 = bond::GetRuntimeSchema<T1>();
    bond::RuntimeSchema s2 = bond::GetRuntimeSchema<T2>();

    bond::bonded<bond::SchemaDef> b1(Serialize(s1));
    bond::bonded<bond::SchemaDef> b2(Serialize(s2));

    bond::bonded<bond::SchemaDef> i1(Incompatible(s1.GetSchema()));
    bond::bonded<bond::SchemaDef> i2(Incompatible(s2.GetSchema()));

    result &= (Check(s1, b2) == forward);
    result &= (Check(b1, s2) == forward);

    result &= (Check(s1, i2) == Error);
    result &= (Check(i1, s2) == Error);

    result &= (Check(s2, b1) == reverse);
    result &= (Check(b2, s1) == reverse);

    result &= (Check(s2, i1) == Error);
    result &= (Check(i2, s1) == Error);

    result &= (CheckTwoWay(s1, b2) == bidirectional);
    result &= (CheckTwoWay(b1, s2) == bidirectional);

    result &= (CheckTwoWay(s1, i2) == Error);
    result &= (CheckTwoWay(i1, s2) == Error);
    
    result &= (Check(s1, s2) == forward);
    result &= (Check(s2, s1) == reverse);   
    result &= (CheckTwoWay(s1, s2) == bidirectional);

    bond::RuntimeSchema ss1 = bond::GetRuntimeSchema<BondStruct<T1> >();
    bond::RuntimeSchema sb1 = bond::GetRuntimeSchema<BondStruct<bond::bonded<T1> > >();
    bond::RuntimeSchema ss2 = bond::GetRuntimeSchema<BondStruct<T2> >();
    bond::RuntimeSchema sb2 = bond::GetRuntimeSchema<BondStruct<bond::bonded<T2> > >();

    // both bonded, always identical
    result &= (Check(sb1, sb2) == True);    
    result &= (Check(sb2, sb1) == True);
    result &= (CheckTwoWay(sb1, sb2) == True);

    // only src bonded, never identical
    result &= (Check(sb2, ss1) == (std::min)(reverse, False));

    // only dst bonded, always different and compatible
    result &= (Check(ss1, sb2) == False);
     
    return result;
}

TEST_CASE_BEGIN(Validation)
{
    UT_AssertIsTrue(Check<type1, type1_identical>(True, True, True));
    UT_AssertIsTrue(Check<type1, type1_optional_to_required>(Error, True, Error));
    UT_AssertIsTrue(Check<type2, type2_identical>(True, True, True));
    UT_AssertIsTrue(Check<type3, type3_identical>(True, True, True));
    UT_AssertIsTrue(Check<type1, type1_derived>(Error, False, Error));
    UT_AssertIsTrue(Check<type1, type1_downgrade_fields>(Error, False, Error));
    UT_AssertIsTrue(Check<type1, type1_removed_optional_field>(False, False, False));
    UT_AssertIsTrue(Check<type2, type2_compatible_base>(Error, False, Error));
    UT_AssertIsTrue(Check<type1, type1_different_field>(Error, Error, Error));
    UT_AssertIsTrue(Check<type1, type1_upgrade_fields>(False, Error, Error));
    UT_AssertIsTrue(Check<type1, type1_removed_required_field>(False, Error, Error));
    UT_AssertIsTrue(Check<type1, type1_required_to_optional>(True, Error, Error));
    UT_AssertIsTrue(Check<type2, type2_incompatible_base>(Error, Error, Error));
    UT_AssertIsTrue(Check<type2, type2_no_base>(Error, Error, Error));
    UT_AssertIsTrue(Check<type4, type4_compatible>(False, Error, Error));
    UT_AssertIsTrue(Check<empty0, depth1>(False, False, False));
    UT_AssertIsTrue(Check<empty0, depth2>(Error, False, Error));
    UT_AssertIsTrue(Check<empty0, depth3>(Error, False, Error));
    UT_AssertIsTrue(Check<depth1, depth2>(Error, False, Error));
    UT_AssertIsTrue(Check<depth1, depth3>(Error, False, Error));
    UT_AssertIsTrue(Check<depth2, depth3>(Error, False, Error));
}
TEST_CASE_END


template <uint16_t N>
void ValidationTests(const char* name)
{
    UnitTestSuite suite(name);
    AddTestCase<TEST_ID(N), Validation>(suite, "Validate tests");
}

void ValidateTest::Initialize()
{
    TEST_COMPACT_BINARY_PROTOCOL(
        ValidationTests<0x2001>("Validation tests");
    );
}


bool init_unit_test()
{
    ValidateTest::Initialize();
    return true;
}

