#include "precompiled.h"
#include <bond/core/container_interface.h>
#include "container_extensibility.h"


template <typename T>
struct SimpleSet
{
    SimpleSet()
    {}

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    SimpleSet(SimpleSet&& other)
        : impl(std::move(other.impl))
    {}
#endif

#ifndef BOOST_NO_CXX11_DEFAULTED_FUNCTIONS
    SimpleSet(const SimpleSet&) = default;
    SimpleSet& operator=(const SimpleSet&) = default;
#endif

    std::set<T> impl;
};


namespace bond
{
    template <typename T>
    struct is_set_container<SimpleSet<T> >
        : std::true_type {};

    template <typename T>
    struct element_type<SimpleSet<T> >
    {
        typedef T type;
    };

    template <typename T>
    struct const_enumerator<SimpleSet<T> >
        : public bond::const_enumerator<std::set<T> >
    {
        const_enumerator(const SimpleSet<T>& set)
            : bond::const_enumerator<std::set<T> >(set.impl)
        {}
    };
};


template <typename T>
uint32_t container_size(const SimpleSet<T>& set)
{
    return static_cast<uint32_t>(set.impl.size());
}

template <typename T>
void clear_set(SimpleSet<T>& set)
{
    set.impl.clear();
}


template <typename T>
void set_insert(SimpleSet<T>& set, const T& item)
{
    set.impl.insert(item);
}


template <typename K, typename T>
struct SimpleMap
{
    SimpleMap()
    {}

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    SimpleMap(SimpleMap&& other)
        : impl(std::move(other.impl))
    {}
#endif

#ifndef BOOST_NO_CXX11_DEFAULTED_FUNCTIONS
    SimpleMap(const SimpleMap&) = default;
    SimpleMap& operator=(const SimpleMap&) = default;
#endif

    std::map<K, T> impl;
};


namespace bond
{
    template <typename K, typename T>
    struct is_map_container<SimpleMap<K, T> >
        : std::true_type {};

    template <typename K, typename T>
    struct element_type<SimpleMap<K, T> >
    {
        typedef typename element_type<typename std::map<K, T> >::type type;
    };

    template <typename K, typename T>
    struct const_enumerator<SimpleMap<K, T> >
        : public bond::const_enumerator<std::map<K, T> >
    {
        const_enumerator(const SimpleMap<K, T>& map)
            : bond::const_enumerator<std::map<K, T> >(map.impl)
        {}
    };
};


template <typename K, typename T>
uint32_t container_size(const SimpleMap<K, T>& map)
{
    return static_cast<uint32_t>(map.impl.size());
}

template <typename K, typename T>
void clear_map(SimpleMap<K, T>& map)
{
    map.impl.clear();
}


template <typename K, typename T>
T& mapped_at(SimpleMap<K, T>& map, const K& key)
{
    return map.impl[key];
}

    
template <typename Reader, typename Writer>
struct SimpleSetTests
{
    template <typename T>
    void operator()(const T&)
    {
        {
            typedef BondStruct<SimpleSet<T> >    Custom;
            typedef BondStruct<std::set<T> >     Standard;

            AllBindingAndMapping<Reader, Writer, Custom, Standard>();
            AllBindingAndMapping<Reader, Writer, Standard, Custom>();
        }
    }
};


template <typename Reader, typename Writer>
struct SimpleMapTests
{
    template <typename T>
    void operator()(const T&)
    {
        {
            typedef BondStruct<SimpleMap<T, std::vector<uint8_t> > > Custom;
            typedef BondStruct<std::map<T, std::vector<uint8_t> > >  Standard;

            AllBindingAndMapping<Reader, Writer, Custom, Standard>();
            AllBindingAndMapping<Reader, Writer, Standard, Custom>();
        }

        {
            typedef BondStruct<SimpleMap<T, SimpleStruct> > Custom;
            typedef BondStruct<std::map<T, SimpleStruct> >  Standard;

            AllBindingAndMapping<Reader, Writer, Custom, Standard>();
            AllBindingAndMapping<Reader, Writer, Standard, Custom>();
        }
    }
};


typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        wstring,
        uint64_t,
        int8_t,
        double,
#endif
        string
    >
    Types;


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(SimpleSetTest)
{
    boost::mpl::for_each<Types>(SimpleSetTests<Reader, Writer>());
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(SimpleMapTest)
{
    boost::mpl::for_each<Types>(SimpleMapTests<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void AssociativeContainerExtensibilityTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), 
        SimpleMapTest, Reader, Writer>(suite, "Custom map tests");

    AddTestCase<TEST_ID(N), 
        SimpleSetTest, Reader, Writer>(suite, "Custom set tests");
}


void ExtensibilityTest::InitializeAssociative()
{
    TEST_SIMPLE_PROTOCOL(
        AssociativeContainerExtensibilityTests<
            0x1201,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Associative container extensibility for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        AssociativeContainerExtensibilityTests<
            0x1202,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Associative container extensibility for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        AssociativeContainerExtensibilityTests<
            0x1203,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Associative container extensibility for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        AssociativeContainerExtensibilityTests<
            0x1204,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Associative container extensibility for Simple JSON");
    );
}
