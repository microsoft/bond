#include "precompiled.h"
#include "serialization_test.h"

template <typename Reader, typename Writer>
TEST_CASE_BEGIN(LargeBlob)
{
    typedef BondStruct<bond::blob> T;

    T blob;
    const uint32_t size = 64 * 1024;
    struct Big
    {
        char big[size];
    };
    boost::shared_ptr<Big> p(new Big);

    blob.field.assign(p.get(), sizeof(Big));
    BindingAndMapping<Reader, Writer, T, T>(blob);

    blob.field.assign(p, sizeof(Big));
    BindingAndMapping<Reader, Writer, T, T>(blob);

    blob.field.assign(p, sizeof(Big)/2, sizeof(Big)/2);
    BindingAndMapping<Reader, Writer, T, T>(blob);

    boost::shared_array<char> a(new char[size]);

    blob.field.assign(a, size);
    BindingAndMapping<Reader, Writer, T, T>(blob);

    blob.field.assign(a, size/2, size/2);
    BindingAndMapping<Reader, Writer, T, T>(blob);
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(OutputBufferBlobs)
{
    Blobs blobs = InitRandom<Blobs>();

    {
        // chain all blobs (minimum size 0 bytes)
        bond::OutputBuffer stream(1000, 1, std::allocator<char>(), 0);
        Writer writer(stream);

        Serialize(blobs, writer);
        std::vector<bond::blob> buffers;
        stream.GetBuffers(buffers);

        if (Reader::magic == bond::SIMPLE_JSON_PROTOCOL)
            UT_AssertAreEqual(std::size_t(1), buffers.size());
        else
            UT_AssertAreEqual(blobs.blobs.size() * 2 + 1, buffers.size());
    }

    {
        // don't chain blobs (minimum size 1000 bytes)
        bond::OutputBuffer stream(1000, 1, std::allocator<char>(), 1000);
        Writer writer(stream);

        Serialize(blobs, writer);
        std::vector<bond::blob> buffers;
        stream.GetBuffers(buffers);
        UT_AssertAreEqual(buffers.size(), std::size_t(1));
    }

    {
        // don't chain blobs (minimum size 1000 bytes but chain size limit set at 0)
        bond::OutputBuffer stream(1000, 1, std::allocator<char>(), 0, 0);
        Writer writer(stream);

        Serialize(blobs, writer);
        std::vector<bond::blob> buffers;
        stream.GetBuffers(buffers);
        UT_AssertAreEqual(buffers.size(), std::size_t(1));
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void BlobTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping1, Reader, Writer, BondStruct<bond::blob> >(suite, "blob deserialization");

#ifndef UNIT_TEST_SUBSET
    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, BondStruct<bond::blob>, BondStruct<std::vector<int8_t> > >(suite, "blob-vector interop");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, BondStruct<std::vector<int8_t> >, BondStruct<bond::blob> >(suite, "vector-blob interop");

    AddTestCase<TEST_ID(N),
        LargeBlob, Reader, Writer>(suite, "large blob");

    AddTestCase<TEST_ID(N),
        OutputBufferBlobs, Reader, Writer>(suite, "OutputBuffer blobs");
#endif
}


void SerializationTest::BlobTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        BlobTests<
            0x1001,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Blob tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        BlobTests<
            0x1002,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Blob tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        BlobTests<
            0x1003,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Blob tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        BlobTests<
            0x1004,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Blob tests for Simple JSON");
    );
}
