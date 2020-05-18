#include "precompiled.h"
#include "allocator_test_reflection.h"
#include "exception_tests.h"

template <typename Reader, typename T, typename BondedType>
void Deserialize(bond::blob data)
{                                                              
    BOOST_STATIC_ASSERT((std::is_same<BondedType, T>::value || std::is_same<BondedType, void>::value));

    Reader reader(data);
    bond::bonded<T> bonded(reader);
    
    T obj;

    // De-serialize the object
    bond::bonded<BondedType>(bonded).Deserialize(obj);
}


template <typename Writer>
struct InvalidPayload
{
    template <typename T>
    static bond::blob PartialField()
    {
        // Payload indicates a field of type T and id 2 and then ends.
        // Attempt to de-serialize or skip the field leads to Eof exception.
        typename Writer::Buffer buffer;
        Writer writer(buffer);
                                                   
        writer.WriteStructBegin(bond::Metadata(), false);
        writer.WriteFieldBegin(bond::get_type_id<T>::value, 2);

        return buffer.GetBuffer();
    }

    template <typename T>
    static bond::blob OomField()
    {
        // Payload indicates a container field with id 2 and _UI32_MAX elements and then ends.
        // Attempt to de-serialize leads to OOM exception.
        typename Writer::Buffer buffer;
        Writer writer(buffer);

        writer.WriteStructBegin(bond::Metadata(), false);
        writer.WriteFieldBegin(bond::get_type_id<T>::value, 2);
        writer.WriteContainerBegin(0xffffffff, bond::get_type_id<typename bond::element_type<T>::type>::value);

        return buffer.GetBuffer();
    }
};


template <typename Reader, typename Writer>
struct MissingFieldExceptionTest
{
    template <typename T>
    void operator()(const T&)
    {
        // The test structure contains 2 required fields of type T. The invalid payload
        // contains only partial second field. With dynamic parser the missing required
        // field leads to missing field exception, and skipping the field during stack
        // unrolling results in Eof exception which is internally caught.
        // With static parser there is no missing field exception and only Eof exception.
        typedef BondStructRequired<T> Type;
        
        bool        exception;
        bond::blob  payload;
        
        payload = InvalidPayload<Writer>::template PartialField<T>();

        // compile-time binding
        try
        {
            exception = false;
            Deserialize<Reader, Type, Type>(payload);
        }
        catch(bond::CoreException&)
        {
            exception = true;
        }
        catch(bond::StreamException&)
        {
            UT_AssertIsTrue(bond::uses_static_parser<Reader>::value);
            exception = true;
        }

        UT_AssertIsTrue(exception);

        // runtime binding
        try
        {
            exception = false;
            Deserialize<Reader, Type, void>(payload);
        }
        catch(bond::CoreException&)
        {
            exception = true;
        }
        catch(bond::StreamException&)
        {
            UT_AssertIsTrue(bond::uses_static_parser<Reader>::value);
            exception = true;
        }

        UT_AssertIsTrue(exception);
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(MissingFieldException)
{
    boost::mpl::for_each<BasicTypes>(MissingFieldExceptionTest<Reader, Writer>());
    boost::mpl::for_each<SkipTypes<double>::type>(MissingFieldExceptionTest<Reader, Writer>());
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(OomException)
{
    typedef BondStructOptional<std::vector<allocator_test::SimpleType, detail::TestAllocator<allocator_test::SimpleType> > > Type;
        
    bool        exception;
    bond::blob  payload;
        
    payload = InvalidPayload<Writer>::template OomField<std::vector<allocator_test::SimpleType> >();

    // compile-time binding
    try
    {
        exception = false;
        Deserialize<Reader, Type, Type>(payload);
    }
    catch(std::exception&)
    {
        exception = true;
    }

    UT_AssertIsTrue(exception);

    // runtime binding
    try
    {
        exception = false;
        Deserialize<Reader, Type, void>(payload);
    }
    catch(std::exception&)
    {
        exception = true;
    }

    UT_AssertIsTrue(exception);
}
TEST_CASE_END


struct transform_exception {};

class Exceptions
    : public bond::SerializingTransform
{
public:
    Exceptions(int when)
        : when(when)
    {}

    void Begin(const bond::Metadata&) const
    {
        MaybeThrow();
    }

    void End() const
    {
        MaybeThrow();
    }

    void UnknownEnd() const
    {
        MaybeThrow();
    }

    template <typename T>
    bool Base(const T& value) const
    {
        return Process(value);
    }

    template <typename T>
    bool Field(uint16_t, const bond::Metadata&, const T& value) const
    {
        return Process(value);
    }

    template <typename T>
    bool UnknownField(uint16_t, const T& value) const
    {
        return Process(value);
    }

    template <typename T>
    void Container(const T& element, uint32_t size) const
    {
        while (size--)
            Process(element);
    }

    template <typename Key, typename T>
    void Container(const Key& key, const T& value, uint32_t size) const
    {
        while (size--)
        {
            Process(key);
            Process(value);
        }
    }

private:
    template <typename Reader, typename T>
    typename boost::enable_if<bond::is_basic_type<T>, bool>::type
    Process(const bond::value<T, Reader>& value) const
    {
        MaybeThrow();
        value.Skip();
        return false;
    }

    template <typename Reader, typename T>
    typename boost::disable_if<bond::is_basic_type<T>, bool>::type
    Process(const bond::value<T, Reader>& value) const
    {
        MaybeThrow();
        Apply(*this, value);
        return false;
    }

    template <typename Reader, typename T>
    bool Process(const bond::bonded<T, Reader>& value) const
    {
        MaybeThrow();
        Apply(*this, value);
        return false;
    }

    void MaybeThrow() const
    {
        if (!when--)
            throw transform_exception();
    }

    mutable int when;
};


template <typename Reader, typename Writer, typename T>
TEST_CASE_BEGIN(TransformException)
{
    typename Writer::Buffer buffer(4096);
    
    Factory<Writer>::Call(buffer, bond::v1, boost::bind(
        bond::Serialize<bond::BuiltInProtocols, T, Writer>, InitRandom<T>(2, 2), boost::placeholders::_1));

    {
        for (int i = 0;; ++i)
        {
            try
            {
                Reader reader(buffer.GetBuffer());
                UT_AssertIsTrue(i < 5000);
                Exceptions exceptions(i);
                Apply(exceptions, bond::bonded<T, Reader&>(reader));
                UT_AssertIsTrue(i > 0);
                break;
            }
            catch(const transform_exception&)
            {
            }
        }

        for (int i = 0;; ++i)
        {
            try
            {
                Reader reader(buffer.GetBuffer());
                UT_AssertIsTrue(i < 5000);
                Exceptions exceptions(i);
                Apply(exceptions, bond::bonded<void, Reader&>(reader, bond::GetRuntimeSchema<T>()));
                UT_AssertIsTrue(i > 0);
                break;
            }
            catch(const transform_exception&)
            {
            }
        }
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void ExceptionTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        OomException, Reader, Writer>(suite, "Out of memory");

    AddTestCase<TEST_ID(N), 
        MissingFieldException, Reader, Writer>(suite, "Eof after missing field");

    AddTestCase<TEST_ID(N), 
        TransformException, Reader, Writer, NestedListsStruct>(suite, "Exceptions in transform");
}


void ExceptionTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        ExceptionTests<
            0x1501,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Exception tests for SimpleBinary"); 
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        ExceptionTests<
            0x1502,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Exception tests for CompactBinary"); 
    );

    TEST_FAST_BINARY_PROTOCOL(
        ExceptionTests<
            0x1503,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Exception tests for FastBinary"); 
    );
}


bool init_unit_test()
{
    ExceptionTest::Initialize();
    return true;
}

