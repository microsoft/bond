#include "custom_protocols.h"
#include "multi_index.h"
#include "precompiled.h"
#include "container_extensibility.h"


template <typename Protocols, typename T, size_t N>
bool Compare(const std::array<T, N>& left, const std::array<T, N>& right)
{
    static_assert(N > 0, "must have non-empty static strings");

    const uint32_t rightLength = std::string_length(right);

    if (std::string_length(left) != rightLength)
    {
        return false;
    }

    for (uint32_t i = 0; i < rightLength; ++i)
    {
        if (!Compare<Protocols>(left[i], static_cast<T>(right[i])))
        {
            return false;
        }
    }

    return true;
}

template <typename Protocols, typename T, size_t N>
bool Compare(const std::array<T, N>& left, const std::basic_string<T>& right)
{
    static_assert(N > 0, "must have non-empty static string");

    if (std::string_length(left) != right.size())
    {
        return false;
    }

    for (typename std::basic_string<T>::size_type i = 0; i < right.size(); ++i)
    {
        if (!Compare<Protocols>(left[i], static_cast<T>(right[i])))
        {
            return false;
        }
    }

    return true;
}

template <typename Protocols>
bool Compare(const WithStaticString& custom, const BondStruct<string>& standard)
{
    return Compare<Protocols>(custom.field, standard.field);
}

template <typename Protocols>
bool Compare(const BondStruct<string>& standard, const WithStaticString& custom)
{
    return Compare<Protocols>(custom.field, standard.field);
}

template <typename Protocols>
bool Compare(const WithStaticWString& custom, const BondStruct<wstring>& standard)
{
    return Compare<Protocols>(custom.field, standard.field);
}

template <typename Protocols>
bool Compare(const BondStruct<wstring>& standard, const WithStaticWString& custom)
{
    return Compare<Protocols>(custom.field, standard.field);
}

template <typename Protocols, typename T>
bool Compare(const WithSimpleList<T>& custom, const BondStruct<list<T> >& standard)
{
    return Equal<Protocols>(custom.field, standard.field);
}

template <typename Protocols, typename T>
bool Compare(const BondStruct<list<T> >& standard, const WithSimpleList<T>& custom)
{
    return Equal<Protocols>(custom.field, standard.field);
}


template <typename Reader, typename Writer>
struct SimpleListTest
{
    template <typename X>
    void operator()(const X&)
    {
        {
            typedef WithSimpleList<X> Custom;
            typedef BondStruct<list<X> > Standard;

            AllBindingAndMapping<Reader, Writer, Custom, Standard>();
            AllBindingAndMapping<Reader, Writer, Standard, Custom>();
        }
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(SimpleListContainerTest)
{
    boost::mpl::for_each<BasicTypes>(SimpleListTest<Reader, Writer>());
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(StringTest)
{
    {
        typedef WithStaticString Custom;
        typedef BondStruct<string> Standard;

        AllBindingAndMapping<Reader, Writer, Custom, Standard>();
        AllBindingAndMapping<Reader, Writer, Standard, Custom>();
    }

    {
        typedef WithStaticWString Custom;
        typedef BondStruct<wstring> Standard;

        AllBindingAndMapping<Reader, Writer, Custom, Standard>();
        AllBindingAndMapping<Reader, Writer, Standard, Custom>();
    }
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(MultiIndexTest)
{
    {
        typedef BondStruct<std::list<uint32_t> > Standard;
        typedef BondStruct<
            boost::multi_index::multi_index_container<
                uint32_t,
                boost::multi_index::indexed_by<
                    boost::multi_index::sequenced<>,
                    boost::multi_index::ordered_non_unique<boost::multi_index::identity<uint32_t> >
                >
            >
        > Custom;

        AllBindingAndMapping<Reader, Writer, Custom, Standard>();
        AllBindingAndMapping<Reader, Writer, Standard, Custom>();
    }

    {
        typedef BondStruct<std::list<SimpleStructView> > Standard;
        typedef BondStruct<
            boost::multi_index::multi_index_container<
                SimpleStructView,
                boost::multi_index::indexed_by<
                    boost::multi_index::sequenced<>,
                    ordered_non_unique_field<SimpleStructView::Schema::var::m_str>,
                    ordered_non_unique_field<SimpleStructView::Schema::var::m_uint64>
                >
            >
        > Custom;

        AllBindingAndMapping<Reader, Writer, Custom, Standard>();
        AllBindingAndMapping<Reader, Writer, Standard, Custom>();
    }
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void ExtensibilityTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        SimpleListContainerTest, Reader, Writer>(suite, "C array based container");

    AddTestCase<TEST_ID(N),
        StringTest, Reader, Writer>(suite, "uint8_t[], uint16_t[] string");

    AddTestCase<TEST_ID(N),
        MultiIndexTest, Reader, Writer>(suite, "boost::multi_index_container");
}


void ExtensibilityTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        ExtensibilityTests<
            0xd01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Container extensibility tests for SimpleBinary");
    );


    TEST_COMPACT_BINARY_PROTOCOL(
        ExtensibilityTests<
            0xd02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Container extensibility tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        ExtensibilityTests<
            0xd03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Container extensibility tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        ExtensibilityTests<
            0xd04,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Container extensibility tests for Simple JSON");
    );
}


bool init_unit_test()
{
    ExtensibilityTest::Initialize();
    ExtensibilityTest::InitializeAssociative();
    return true;
}
