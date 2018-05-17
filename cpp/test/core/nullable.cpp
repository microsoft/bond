#include "precompiled.h"
#include "capped_allocator_tests_generated/allocator_test_types.h"
#include "serialization_test.h"

using namespace std;

template <typename Reader, typename Writer>
struct NullableTest
{
    template <typename X>
    void operator()(const X&)
    {
        typedef BondStruct<bond::nullable<X> > T;

        AllBindingAndMapping<Reader, Writer, T>();
    }
};

class Type
{
public:
    explicit Type()
        : value(0)
    { }
    int value;
};

inline bool operator==(const Type& t1, const Type& t2)
{
    return t1.value == t2.value;
}

template <typename Reader, typename Writer>
TEST_CASE_BEGIN(AllBasicTypesNullables)
{
    typedef boost::mpl::copy<BasicTypes, boost::mpl::front_inserter<ListTypes<string>::type> >::type  Types;

    boost::mpl::for_each<Types>(NullableTest<Reader, Writer>());
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(StructNullables)
{
    typedef boost::mpl::list<SimpleStruct, StructWithNullables>::type Types;

    boost::mpl::for_each<Types>(NullableTest<Reader, Writer>());
}
TEST_CASE_END

template <typename T, typename S, typename L>
void NullableTests(T& x, T& y, const S& s1, const L& l1)
{
    Type c;
    c.value = 100;

    bond::nullable<Type> p;
    bond::nullable<Type> q;

    UT_AssertIsTrue(x.nullable_uint32.empty());
    UT_AssertIsTrue(x.nullable_list.empty());
    UT_AssertIsTrue(x.nullable_struct.empty());
    UT_AssertIsTrue(p.empty());

    UT_AssertIsFalse(x.nullable_uint32.hasvalue());
    UT_AssertIsFalse(x.nullable_list.hasvalue());
    UT_AssertIsFalse(x.nullable_struct.hasvalue());
    UT_AssertIsFalse(p.hasvalue());

    x.nullable_uint32 = bond::nullable<uint32_t>(10);
    x.nullable_list = l1;
    x.nullable_struct = s1;
    p = bond::nullable<Type>(c);

    UT_AssertIsFalse(x.nullable_uint32.empty());
    UT_AssertIsFalse(x.nullable_list.empty());
    UT_AssertIsFalse(x.nullable_struct.empty());
    UT_AssertIsFalse(p.empty());

    UT_AssertIsTrue(x.nullable_uint32.hasvalue());
    UT_AssertIsTrue(x.nullable_list.hasvalue());
    UT_AssertIsTrue(x.nullable_struct.hasvalue());
    UT_AssertIsTrue(p.hasvalue());

    swap(x, y);
    swap(p, q);

    UT_AssertIsTrue(x.nullable_uint32.empty());
    UT_AssertIsTrue(x.nullable_list.empty());
    UT_AssertIsTrue(x.nullable_struct.empty());
    UT_AssertIsTrue(p.empty());

    UT_AssertIsFalse(x.nullable_uint32.hasvalue());
    UT_AssertIsFalse(x.nullable_list.hasvalue());
    UT_AssertIsFalse(x.nullable_struct.hasvalue());
    UT_AssertIsFalse(p.hasvalue());


    UT_AssertIsFalse(y.nullable_uint32.empty());
    UT_AssertIsFalse(y.nullable_list.empty());
    UT_AssertIsFalse(y.nullable_struct.empty());
    UT_AssertIsFalse(q.empty());

    UT_AssertIsTrue(y.nullable_uint32.hasvalue());
    UT_AssertIsTrue(y.nullable_list.hasvalue());
    UT_AssertIsTrue(y.nullable_struct.hasvalue());
    UT_AssertIsTrue(q.hasvalue());

    x = y;
    p = q;

    UT_AssertIsTrue(x == y);
    UT_AssertIsTrue(p == q);

    x.nullable_list->push_back(3.14f);
    x.nullable_struct->m_int16 = 314;
    (*p).value = 100;

    y.nullable_list->push_back(3.14f);
    (*y.nullable_struct).m_int16 = 314;
    q = bond::nullable<Type>(c);

    UT_AssertIsTrue(x == y);
    UT_AssertIsTrue(p == q);

    x.nullable_uint32.reset();
    x.nullable_list.reset();
    p.reset();

    UT_AssertIsTrue(x != y);
    UT_AssertIsTrue(p != q);

    x.nullable_uint32.set() = 10;
    x.nullable_list.set(*y.nullable_list);
    p.set(*q);

    UT_AssertIsTrue(x == y);
    UT_AssertIsTrue(p == q);

    std::list<double> l(5);
    bond::nullable<std::list<double> > nl(l);

    UT_AssertIsFalse(nl.empty());
    UT_AssertIsTrue(nl.value() == l);

    float f = 3.14f;
    bond::nullable<float> nf(f);
    bond::nullable<bond::nullable<float> > nnf(nf);
    bond::nullable<bond::nullable<bond::nullable<float> > > nnnf(nnf);

    UT_AssertIsFalse(nf.empty());
    UT_AssertIsTrue(nf.value() == f);
    UT_AssertIsFalse(nnf.empty());
    UT_AssertIsTrue(nnf.value().value() == f);
    UT_AssertIsFalse(nnnf.empty());
    UT_AssertIsTrue(nnnf.value().value().value() == f);

    UT_AssertIsTrue(x.nullable_nullable_uint32.empty());
    UT_AssertIsTrue(x.nullable_nullable_list.empty());
    UT_AssertIsTrue(x.nullable_nullable_struct.empty());

    x.nullable_nullable_uint32.set();
    x.nullable_nullable_list.set();
    x.nullable_nullable_struct.set();

    UT_AssertIsFalse(x.nullable_nullable_uint32.empty());
    UT_AssertIsFalse(x.nullable_nullable_list.empty());
    UT_AssertIsFalse(x.nullable_nullable_struct.empty());

    x.nullable_nullable_uint32.reset();
    x.nullable_nullable_list.reset();
    x.nullable_nullable_struct.reset();

    UT_AssertIsTrue(x.nullable_nullable_uint32.empty());
    UT_AssertIsTrue(x.nullable_nullable_list.empty());
    UT_AssertIsTrue(x.nullable_nullable_struct.empty());

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    x.nullable_uint32 = bond::nullable<uint32_t>(10);
    x.nullable_list = l1;
    x.nullable_struct = s1;
    p = bond::nullable<Type>(c);

    y.nullable_uint32 = std::move(x.nullable_uint32);
    y.nullable_list = std::move(x.nullable_list);
    y.nullable_struct = std::move(x.nullable_struct);
    q = std::move(p);

    UT_AssertIsTrue(x.nullable_uint32.empty());
    UT_AssertIsTrue(x.nullable_list.empty());
    UT_AssertIsTrue(x.nullable_struct.empty());
    UT_AssertIsTrue(p.empty());

    UT_AssertIsFalse(y.nullable_uint32.empty());
    UT_AssertIsFalse(y.nullable_list.empty());
    UT_AssertIsFalse(y.nullable_struct.empty());
    UT_AssertIsFalse(q.empty());

    x.nullable_uint32.set(std::move(y.nullable_uint32.value()));
    x.nullable_list.set(std::move(y.nullable_list.value()));
    x.nullable_struct.set(std::move(y.nullable_struct.value()));
    p.set(std::move(q.value()));

    UT_AssertIsFalse(x.nullable_uint32.empty());
    UT_AssertIsFalse(x.nullable_list.empty());
    UT_AssertIsFalse(x.nullable_struct.empty());
    UT_AssertIsFalse(p.empty());
#endif
}


TEST_CASE_BEGIN(NullableInterface)
{
    StructWithNullables x, y;

    list<float> l;
    SimpleStruct s;

    bond::nullable<list<float> > l1(l);
    bond::nullable<SimpleStruct> s1(s);

    NullableTests(x, y, s1, l1);
}
TEST_CASE_END


TEST_CASE_BEGIN(NullableAllocators)
{
    using capped_allocator_tests::NullableFields;
    using capped_allocator_tests::SimpleType;

    bond::ext::capped_allocator<> a1{ (std::numeric_limits<std::uint32_t>::max)() };
    bond::ext::capped_allocator<> a2{ (std::numeric_limits<std::uint32_t>::max)() };

    NullableFields x(a1);
    NullableFields y(a1);

    list<float, std::allocator_traits<bond::ext::capped_allocator<> >::rebind_alloc<float> > l(a2);
    SimpleType s(a2);

    bond::nullable<list<float,
        std::allocator_traits<bond::ext::capped_allocator<> >::rebind_alloc<float> > > l1(l);
    bond::nullable<SimpleType> s1(s, a2);

    NullableTests(x, y, s1, l1);

    bond::nullable<std::set<bool, std::less<bool>,
        std::allocator_traits<bond::ext::capped_allocator<> >::rebind_alloc<bool> > > n1(a1);
    UT_AssertIsTrue(n1.empty());

    NullableFields ns1(a1);
    NullableFields ns2(a2);

    ns1.nullable_uint32.set();
    ns1.nullable_list.set();
    ns1.nullable_struct.set();

    UT_AssertIsFalse(ns1.nullable_uint32.empty());
    UT_AssertIsFalse(ns1.nullable_list.empty());
    UT_AssertIsFalse(ns1.nullable_struct.empty());

    UT_AssertIsTrue(ns2.nullable_uint32.empty());
    UT_AssertIsTrue(ns2.nullable_list.empty());
    UT_AssertIsTrue(ns2.nullable_struct.empty());

    swap(ns1, ns2);

    UT_AssertIsFalse(ns2.nullable_uint32.empty());
    UT_AssertIsFalse(ns2.nullable_list.empty());
    UT_AssertIsFalse(ns2.nullable_struct.empty());

    UT_AssertIsTrue(ns1.nullable_uint32.empty());
    UT_AssertIsTrue(ns1.nullable_list.empty());
    UT_AssertIsTrue(ns1.nullable_struct.empty());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void BasicTypesNullableTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        AllBasicTypesNullables, Reader, Writer>(suite, "Nullable types and containers");

    AddTestCase<TEST_ID(N),
        StructNullables, Reader, Writer>(suite, "Nullable of structs");
}


void NullableTestsInit()
{
    TEST_SIMPLE_PROTOCOL(
        BasicTypesNullableTests<
            0x501,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Nullable tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        BasicTypesNullableTests<
            0x502,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Nullable tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        BasicTypesNullableTests<
            0x503,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Nullable tests for FastBinary");
    );

    TEST_SIMPLE_JSON_PROTOCOL(
        BasicTypesNullableTests<
            0x504,
            bond::SimpleJsonReader<bond::InputBuffer>,
            bond::SimpleJsonWriter<bond::OutputBuffer> >("Nullable tests for Simple JSON");
    );

    UnitTestSuite suite("Nullable non-serialization test");

    AddTestCase<TEST_ID(0x506),
        NullableInterface>(suite, "Nullable interface");

    AddTestCase<TEST_ID(0x506),
        NullableAllocators>(suite, "Nullable allocators");
}

bool init_unit_test()
{
    NullableTestsInit();
    return true;
}
