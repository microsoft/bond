#include "precompiled.h"
#include "maybe_tests.h"

#include "allocators.h"

#include <boost/none.hpp>
#include <boost/optional/optional.hpp>

#include <initializer_list>
#include <iterator>
#include <map>
#include <vector>

void ConstInterface(const bond::maybe<int8_t>& x)
{
    bond::maybe<int8_t> z;
    int8_t y;

    UT_AssertIsFalse(x.is_nothing());
    UT_AssertIsTrue(x);
    UT_AssertIsFalse(z == x);

    y = x.value();

    UT_AssertIsTrue(x == y);
    UT_AssertIsTrue(y == x);
    UT_AssertIsFalse(z == x);
    UT_AssertIsTrue(z != x);

    z = x;

    UT_AssertIsTrue(z == x);
    UT_AssertIsFalse(z != x);
}


TEST_CASE_BEGIN(MaybeInterface)
{
    bond::maybe<int8_t> x;
    int8_t y;

    UT_AssertIsTrue(x.is_nothing());
    UT_AssertIsFalse(x);
    UT_AssertThrows(x.value(), bond::CoreException);

    x.set_value() = 10;

    ConstInterface(x);

    UT_AssertIsFalse(x.is_nothing());
    UT_AssertIsTrue(x);

    y = x.value();
    UT_AssertIsTrue(x == y);
    UT_AssertIsTrue(y == x);

    x.set_nothing();

    UT_AssertIsTrue(x.is_nothing());
    UT_AssertIsFalse(x);

    x = y;
    UT_AssertIsTrue(x == y);
    UT_AssertIsTrue(y == x);

    bond::maybe<int8_t> z;

    std::swap(x, z);

    UT_AssertIsFalse(z == x);
    UT_AssertIsTrue(z == y);
    UT_AssertIsTrue(x.is_nothing());
    UT_AssertIsFalse(x);

    x = z;

    UT_AssertIsTrue(z == x);

    bond::maybe<std::map<double, std::string>> map;

    UT_AssertIsTrue(map.is_nothing());
    UT_AssertIsFalse(map);

    map.set_value()[2.71] = "e";
    map.set_value()[3.14] = "pi";

    UT_AssertIsFalse(map.is_nothing());
    UT_AssertIsTrue(map);
    UT_AssertIsTrue(map.value().size() == 2);

    std::map<double, std::string> map2;

    std::swap(map2, map.value());
    std::swap(map.value(), map2);

    const auto numbers = { 1, 2, 3, 4, 5 };
    bond::maybe<std::vector<int>> mVec;
    mVec.emplace(std::begin(numbers), std::end(numbers));
    UT_AssertIsFalse(mVec.is_nothing());
    UT_AssertIsTrue(mVec);
    UT_AssertIsTrue(mVec.value().size() == 5);

    MaybeStruct<double> m1;
    MaybeStruct<double> m2(m1);

    UT_AssertIsTrue(m1 == m2);

    m1.field = 3.14;

    UT_AssertIsFalse(m1 == m2);

    std::swap(m2, m1);

    UT_AssertIsFalse(m1 == m2);
    UT_AssertIsTrue(m1.field.is_nothing());
    UT_AssertIsFalse(m1.field);

    m1 = m2;

    UT_AssertIsTrue(m1 == m2);

    MaybeStruct<double> m3(m1);

    UT_AssertIsTrue(m3 == m2);
}
TEST_CASE_END

TEST_CASE_BEGIN(MovedFromIsNothing)
{
    {
        bond::maybe<bool> m(false);
        UT_AssertIsFalse(m.is_nothing());

        bond::maybe<bool> sink(std::move(m));
        UT_AssertIsTrue(m.is_nothing());

        UT_AssertIsFalse(sink.is_nothing());
        UT_AssertIsFalse(sink.value());
    }

    {
        bond::maybe<std::unique_ptr<int>> moveOnly{};
        moveOnly.emplace(new int { 42 });
        UT_AssertIsFalse(moveOnly.is_nothing());

        bond::maybe<std::unique_ptr<int>> moveOnlySink(std::move(moveOnly));
        UT_AssertIsTrue(moveOnly.is_nothing());

        UT_AssertIsFalse(moveOnlySink.is_nothing());
        UT_AssertIsTrue(*moveOnlySink.value() == 42);
    }
}
TEST_CASE_END

struct UsesAllocator
{
    using allocator_type = allocator_with_state<>;

    UsesAllocator() = default;
    UsesAllocator(const UsesAllocator&) = default;
    UsesAllocator(UsesAllocator&&) = default;

    UsesAllocator(const allocator_type& alloc)
        : constructed_alloc(alloc)
    { }

    UsesAllocator(const UsesAllocator&, const allocator_type& alloc)
        : copied_alloc(alloc)
    { }

    UsesAllocator(UsesAllocator&&, const allocator_type& alloc)
        : moved_alloc(alloc)
    { }

    UsesAllocator& operator=(const UsesAllocator&) = default;

    boost::optional<allocator_type> constructed_alloc{};
    boost::optional<allocator_type> copied_alloc{};
    boost::optional<allocator_type> moved_alloc{};
};

TEST_CASE_BEGIN(AllocatorPropagated)
{
    allocator_with_state<> a1 { std::make_shared<int>() };
    allocator_with_state<> a2 { std::make_shared<int>() };

    bond::maybe<UsesAllocator> m { a1 };
    m.set_value();
    BOOST_REQUIRE(!m.is_nothing());
    BOOST_CHECK(m.value().constructed_alloc == a1);
    BOOST_CHECK(!m.value().copied_alloc);
    BOOST_CHECK(!m.value().moved_alloc);

    {
        bond::maybe<UsesAllocator> mCopy { m };
        BOOST_REQUIRE(!mCopy.is_nothing());
        BOOST_CHECK(mCopy.value().constructed_alloc == a1);
        BOOST_CHECK(!mCopy.value().copied_alloc);
        BOOST_CHECK(!mCopy.value().moved_alloc);
    }

    {
        bond::maybe<UsesAllocator> mCopyAlloc { m, a2 };
        BOOST_REQUIRE(!mCopyAlloc.is_nothing());
        BOOST_CHECK(!mCopyAlloc.value().constructed_alloc);
        BOOST_CHECK(mCopyAlloc.value().copied_alloc == a2);
        BOOST_CHECK(!mCopyAlloc.value().moved_alloc);
    }

    {
        bond::maybe<UsesAllocator> mMove { std::move(m) };
        BOOST_REQUIRE(!mMove.is_nothing());
        BOOST_CHECK(mMove.value().constructed_alloc == a1);
        BOOST_CHECK(!mMove.value().copied_alloc);
        BOOST_CHECK(!mMove.value().moved_alloc);
    }

    // We just moved from m, so we need to re-create it for the next test.
    m = bond::maybe<UsesAllocator> { a1 };
    m.set_value();
    BOOST_REQUIRE(!m.is_nothing());
    BOOST_CHECK(m.value().constructed_alloc == a1);
    BOOST_CHECK(!m.value().copied_alloc);
    BOOST_CHECK(!m.value().moved_alloc);

    {
        bond::maybe<UsesAllocator> mMoveAlloc { std::move(m), a2 };
        BOOST_REQUIRE(!mMoveAlloc.is_nothing());
        BOOST_CHECK(!mMoveAlloc.value().constructed_alloc);
        BOOST_CHECK(!mMoveAlloc.value().copied_alloc);
        BOOST_CHECK(mMoveAlloc.value().moved_alloc == a2);
    }

    {
        bond::maybe<UsesAllocator> hasAlloc{ a1 };
        hasAlloc.emplace(); // explicitly create without an allocator
        BOOST_REQUIRE(!hasAlloc.is_nothing());
        BOOST_CHECK(!hasAlloc.value().constructed_alloc);
        BOOST_CHECK(!hasAlloc.value().copied_alloc);
        BOOST_CHECK(!hasAlloc.value().moved_alloc);
    }

    {
        bond::maybe<UsesAllocator> hasAlloc{ a1 };
        hasAlloc = UsesAllocator{ }; // assign from instance without allocator
        BOOST_REQUIRE(!hasAlloc.is_nothing());
        BOOST_CHECK(!hasAlloc.value().constructed_alloc);
        BOOST_CHECK(!hasAlloc.value().copied_alloc);
        BOOST_CHECK(!hasAlloc.value().moved_alloc);
    }

    {
        UsesAllocator orig{ a1 };
        bond::maybe<UsesAllocator> copyAlloc{ orig, a2 };
        BOOST_REQUIRE(!copyAlloc.is_nothing());
        BOOST_CHECK(!copyAlloc.value().constructed_alloc);
        BOOST_CHECK(copyAlloc.value().copied_alloc == a2);
        BOOST_CHECK(!copyAlloc.value().moved_alloc);
    }

    {
        bond::maybe<UsesAllocator> moveAlloc{ UsesAllocator{ a1 }, a2 };
        BOOST_REQUIRE(!moveAlloc.is_nothing());
        BOOST_CHECK(!moveAlloc.value().constructed_alloc);
        BOOST_CHECK(!moveAlloc.value().copied_alloc);
        BOOST_CHECK(moveAlloc.value().moved_alloc == a2);
    }
}
TEST_CASE_END


template <typename Reader, typename Writer, typename Enable = void>
struct MaybeBindingAndMapping;


template <typename Reader, typename Writer>
struct MaybeBindingAndMapping<Reader, Writer, typename boost::enable_if<bond::may_omit_fields<Writer> >::type>
{
    template <typename X>
    void operator()(const X&)
    {
        {
            typedef MaybeStructOptional<X> T;

            AllBindingAndMapping<Reader, Writer, T>();
        }

        {
            typedef MaybeStruct<X> T;

            UT_AssertThrows((Binding<Reader, Writer, T, T, T>(T())), bond::CoreException);
        }
    }
};


template <typename Reader, typename Writer>
struct MaybeBindingAndMapping<Reader, Writer, typename boost::disable_if<bond::may_omit_fields<Writer> >::type>
{
    template <typename X>
    void operator()(const X&)
    {
        {
            typedef MaybeStruct<X> T;

            Binding<Reader, Writer, T, T, T>(InitRandom<T>());
            Mapping<Reader, Writer, T, T, T>(InitRandom<T>());

            UT_AssertThrows((Binding<Reader, Writer, T, T, T>(T())), bond::CoreException);
        }

        {
            typedef MaybeStructOptional<X> T;

            UT_AssertThrows((Binding<Reader, Writer, T, T, T>(T())), bond::CoreException);
        }
    }
};


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(MaybeDeserialization)
{
    typedef boost::mpl::copy<BasicTypes, boost::mpl::front_inserter<ListTypes<string>::type> >::type  Types;

    boost::mpl::for_each<Types>(MaybeBindingAndMapping<Reader, Writer>());

    Binding<Reader, Writer, Nothing, Nothing, Nothing>(InitRandom<Nothing>());
    Mapping<Reader, Writer, Nothing, Nothing, Nothing>(InitRandom<Nothing>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void MaybeTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), MaybeDeserialization, Reader, Writer>(suite, "maybe de/serialization");
}


void MaybeTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        MaybeTests<
            0x1a01,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Maybe tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        MaybeTests<
            0x1a02,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Maybe tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        MaybeTests<
            0x1a03,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Maybe tests for FastBinary");
    );

    UnitTestSuite suite("Protocol independent maybe tests");

    AddTestCase<TEST_ID(0x1a05), MaybeInterface>(suite, "APIs");
    AddTestCase<TEST_ID(0x1a06), MovedFromIsNothing>(suite, "APIs");
    AddTestCase<TEST_ID(0x1a07), AllocatorPropagated>(suite, "APIs");
}


bool init_unit_test()
{
    MaybeTest::Initialize();
    return true;
}
