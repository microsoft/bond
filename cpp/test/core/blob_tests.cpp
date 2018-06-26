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

void check_blob_is_empty(const bond::blob& b)
{
    // workaround: old compilers won't let us print nullptr, while new ones won't
    // compare nullptrs to ints

    void* null = 0;
    UT_AssertAreEqual(b.data(), null);
    UT_AssertAreEqual(b.size(), 0u);
    UT_AssertAreEqual(b.content(), null);
    UT_AssertIsTrue(b.empty());
}

TEST_CASE_BEGIN(BlobMoveAssign)
{
    using bond::blob;

    const uint32_t size = 32;

    // move default-constructed blob
    {
        auto data = boost::make_shared<std::array<char,size>>();
        blob a{data, size};
        blob b;

        UT_AssertAreEqual(data.use_count(), 2);

        a = std::move(b);
        
        check_blob_is_empty(a);
        check_blob_is_empty(b);

        UT_AssertAreEqual(data.use_count(), 1);
    }
    // move from instance with shared ptr
    {
        // move from other instance
        auto data = boost::make_shared<std::array<char, size>>();
        blob a;
        blob b{data, size};

        UT_AssertAreEqual(data.use_count(), 2);

        a = std::move(b);

        UT_AssertAreEqual(a.data(), data.get());
        UT_AssertAreEqual(a.size(), size);

        check_blob_is_empty(b);

        UT_AssertAreEqual(data.use_count(), 2);
    }
    // move from raw memory
    {
        std::array<char, size> p1;
        std::array<char, size> p2;
        blob a{&p1, size};
        blob b{&p2, size};
        blob c;

        a = std::move(b);
        UT_AssertAreEqual(a.data(), &p2);
        UT_AssertAreEqual(a.size(), size);

        check_blob_is_empty(b);

        // move to empty blob
        c = std::move(a);
        UT_AssertAreEqual(c.data(), &p2);
        UT_AssertAreEqual(c.size(), size);

        check_blob_is_empty(a);
    }
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127) // conditional expression is constant
#endif // _MSC_VER

        if (Reader::magic == bond::SIMPLE_JSON_PROTOCOL)
            UT_AssertAreEqual(std::size_t(1), buffers.size());
        else
            UT_AssertAreEqual(blobs.blobs.size() * 2 + 1, buffers.size());

#ifdef _MSC_VER
#pragma warning(pop)
#endif // _MSC_VER
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

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, BondStruct<bond::blob>, BondStruct<std::vector<int8_t> > >(suite, "blob-vector interop");

    AddTestCase<TEST_ID(N),
        AllBindingAndMapping2, Reader, Writer, BondStruct<std::vector<int8_t> >, BondStruct<bond::blob> >(suite, "vector-blob interop");

    AddTestCase<TEST_ID(N),
        LargeBlob, Reader, Writer>(suite, "large blob");

    AddTestCase<TEST_ID(N),
        OutputBufferBlobs, Reader, Writer>(suite, "OutputBuffer blobs");

    AddTestCase<TEST_ID(N),
        BlobMoveAssign>(suite, "BlobMoveAssign");
}


void BlobTestsInit()
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

bool init_unit_test()
{
    BlobTestsInit();
    return true;
}
