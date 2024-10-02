#include "precompiled.h"
#include "security.h"
#include <stdio.h>

#include "security_reflection.h"
#include "security_types.h"

using namespace security;
typedef uint32_t uint;

TEST_CASE_BEGIN(SecurityBadAlloc_VectorPrimitive)
{
    const unsigned char pMaliciousPayload[] = { 
        0xA5, 0xA5, 0x4B, 0x0D, 0x4B, 0x0D, 0x00, 0x41, 
        0x4B, 0xE7, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE };

    // create our object based on our fuzzed bond types file
    Struct<std::string, double, Base<std::wstring> > obj;

    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::CoreException);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityBadAlloc_VectorStruct)
{
    // The payload specifies a lot of items in Struct::items but encodes only one.
    const unsigned char pMaliciousPayload[] = {
       0x0A, 0x00, 0x01, 0x05, 0x03, 0x25, 0x04, 0x4B,
       0x0A, 0xFF, 0xFF, 0xFF, 0x0F, 0x05, 0x7F, 0x00, 
       0x00 };

    Struct<uint, Base<uint>, uint> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    // We will reach EOF while reading the second item in Struct::items, expect a throw.
    UT_AssertThrows(bond::Deserialize(reader, obj), bond::StreamException);

    // We must not allocate more fields than are encoded + 1.
    UT_AssertIsTrue(obj.items.size() <= 2);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityBadAlloc_MapPrimitive)
{
    const unsigned char pMaliciousPayload[] = {
      0x0d, 0x10, 0x10, 0x7f, 0x02, 0xd0, 0x0f, 0x00 };

    Base<map<int, int>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::StreamException);

    // We must not allocate more fields than are encoded + 1.
    UT_AssertIsTrue(obj.x.size() <= 2);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityBadAlloc_SetPrimitive)
{
    const unsigned char pMaliciousPayload[] = {
        0x0c, 0x10, 0x7f, 0x02, 0x00 };

    Base<set<int>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::CoreException);

    // We must not allocate more fields than are encoded + 1.
    UT_AssertIsTrue(obj.x.size() <= 2);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityBadAlloc_ListPrimitive)
{
    const unsigned char pMaliciousPayload[] = {
      0x0b, 0x07, 0x7f, 0x00, 0x00, 0x80, 0x3f, 0x00 };

    Base<list<float>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::CoreException);

    // We must not allocate more fields than are encoded + 1.
    UT_AssertIsTrue(obj.x.size() <= 2);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityBadAlloc_ListStruct)
{
    const unsigned char pMaliciousPayload[] = {
      0x0b, 0x0a, 0x7f, 0x10, 0x02, 0x01, 0x10, 0x01, 
      0x30, 0x04, 0x00, 0x00 };

    Base<list<Struct<int, int, int>>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::CoreException);

    // We must not allocate more fields than are encoded + 1.
    UT_AssertIsTrue(obj.x.size() <= 2);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityBadAlloc_MapStruct)
{
    const unsigned char pMaliciousPayload[] = {
      0x0d, 0x10, 0x0a, 0x7f, 0x02, 0x07, 0x00, 0x00, 
      0x80, 0x3f, 0x01, 0x07, 0x00, 0x00, 0x40, 0x40,
      0x27, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00 };

    Base<map<int, Struct<float, float, float>>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::StreamException);

    // We must not allocate more fields than are encoded + 1.
    UT_AssertIsTrue(obj.x.size() <= 2);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityNullable_MustSkip)
{
    // We put 2 items into a nullable which may have only 0 or 1. The test
    // makes sure that the remaining items are skipped and do not corrupt
    // the rest of deserialization.
    const unsigned char pMaliciousPayload[] = {
      0x01, 0x0b, 0x10, 0x02, 0x08, 0x0a, 0x30, 0x02,
      0x00 };

    Struct<int, int, bond::nullable<int>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);
    bond::Deserialize(reader, obj);

    UT_AssertIsTrue(obj.x == 0);
    UT_AssertIsTrue(obj.y == 1);
    UT_AssertIsTrue(obj.n.hasvalue() && (obj.n.value() == 4 || obj.n.value() == 5));
    UT_AssertIsTrue(obj.items.size() == 0);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityNullable_InsufficientBuffer)
{
    // Payload spcifies a nullable<int> with 127 items, while there are only
    // 4 Bytes left in the buffer.
    const unsigned char pMaliciousPayload[] = {
      0x01, 0x0b, 0x10, 0x7f, 0x08, 0x30, 0x02,
      0x00 };

    Struct<int, int, bond::nullable<int>> obj;
    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::CoreException);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityCpuDos_FastBinary)
{
    const unsigned char pMaliciousPayload[] = { 
        0x0D, 0x00, 0x96, 0x08, 0x0E, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF };

    // create our object based on our fuzzed bond types file
    Struct<std::string, double, Base<std::wstring> > obj;

    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::FastBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::StreamException);
}
TEST_CASE_END

TEST_CASE_BEGIN(SecurityCpuDos_CompactBinary)
{
    const unsigned char pMaliciousPayload[] = {
        0x01, 0x2D, 0x0E, 0x08, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xAB, 
        0x09};

    // create our object based on our fuzzed bond types file
    Struct<std::string, double, Base<std::wstring> > obj;

    bond::InputBuffer input_buffer(static_cast<const void*>(pMaliciousPayload), sizeof(pMaliciousPayload));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input_buffer);

    UT_AssertThrows(bond::Deserialize(reader, obj), bond::StreamException);
}
TEST_CASE_END

void SecurityTest::Initialize()
{
    UnitTestSuite suite("Security tests");
    AddTestCase<TEST_ID(0x2501), SecurityCpuDos_FastBinary>(suite, "CpuDos_FastBinary");
    AddTestCase<TEST_ID(0x2502), SecurityCpuDos_CompactBinary>(suite, "CpuDos_CompactBinary");
    
    AddTestCase<TEST_ID(0x2503), SecurityBadAlloc_VectorPrimitive>(suite, "BadAlloc_VectorPrimitive");
    AddTestCase<TEST_ID(0x2504), SecurityBadAlloc_VectorStruct>(suite, "BadAlloc_VectorStruct");
    AddTestCase<TEST_ID(0x2505), SecurityBadAlloc_MapPrimitive>(suite, "BadAlloc_MapPrimitive");
    AddTestCase<TEST_ID(0x2506), SecurityBadAlloc_MapStruct>(suite, "BadAlloc_MapStruct");
    AddTestCase<TEST_ID(0x2507), SecurityBadAlloc_SetPrimitive>(suite, "BadAlloc_SetPrimitive");
    AddTestCase<TEST_ID(0x2508), SecurityBadAlloc_ListPrimitive>(suite, "BadAlloc_ListPrimitive");
    AddTestCase<TEST_ID(0x2508), SecurityBadAlloc_ListStruct>(suite, "BadAlloc_ListStruct");

    AddTestCase<TEST_ID(0x2509), SecurityNullable_MustSkip>(suite, "Nullable_MustSkip");
    AddTestCase<TEST_ID(0x250a), SecurityNullable_InsufficientBuffer>(suite, "Nullable_InsufficientBuffer");
}


bool init_unit_test()
{
    SecurityTest::Initialize();
    return true;
}
