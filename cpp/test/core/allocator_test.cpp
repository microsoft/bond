#include "precompiled.h"
#include "allocator_test_reflection.h"
#include "serialization_test.h"

template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(AllocatorTest)     
{
    TestAllocator alloc;
    boost::shared_ptr<T> from = boost::allocate_shared<T>(alloc, alloc);
    
    InitRandom(*from);
    
    // Serialize
    typename Writer::Buffer buffer(alloc);
    Writer writer(buffer);
    bond::Serialize(*from, writer);
        
    bond::blob data = buffer.GetBuffer();

    // Compile-time deserialize
    {
        boost::shared_ptr<T> to = boost::allocate_shared<T>(alloc, alloc);

        Reader reader(data);
        bond::Deserialize(reader, *to);
        UT_Compare(*from, *to);

        boost::shared_ptr<T> x = boost::allocate_shared<T>(alloc, alloc);
        boost::shared_ptr<T> y = boost::allocate_shared<T>(alloc, alloc);
        
        *x = *to;
        
        y->swap(*to);
        
        UT_Compare(*x, *y);
    }

    // Runtime-time deserialize
    {
        boost::shared_ptr<T> to = boost::allocate_shared<T>(alloc, alloc);

        Reader reader(data);
        bond::bonded<void> bonded(reader, bond::GetRuntimeSchema<T>());

        bonded.Deserialize(*to);
        UT_Compare(*from, *to);
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void AllocatorTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        AllocatorTest, Reader, Writer, allocator_test::Struct>(suite, "allocator_test::Struct");
}


void AllocatorTestsInit()
{
    typedef bond::OutputMemoryStream<TestAllocator> OutputStream;

    TEST_SIMPLE_PROTOCOL(
        AllocatorTests<
            0x1901,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<OutputStream> >("Allocator tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        AllocatorTests<
            0x1902,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<OutputStream> >("Allocator tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        AllocatorTests<
            0x1903,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<OutputStream> >("Allocator tests for FastBinary");
    );
}

bool init_unit_test()
{
    AllocatorTestsInit();
    return true;
}

