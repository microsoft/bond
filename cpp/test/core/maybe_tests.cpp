#include "precompiled.h"
#include "maybe_tests.h"

#include "allocators.h"

#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/static_assert.hpp>

#include <initializer_list>
#include <iterator>
#include <map>
#include <vector>

void ConstInterface(const bond::maybe<int8_t>& x)
{
    // BOOST_TEST_CONTEXT("Const interface")
    {
        bond::maybe<int8_t> z;
        int8_t y;

        UT_AssertIsFalse(x.is_nothing());
        UT_AssertIsTrue(x);
        UT_AssertIsFalse(!x);
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
}


TEST_CASE_BEGIN(MaybeInterface)
{
    bond::maybe<int8_t> x;
    int8_t y;

    UT_AssertIsTrue(x.is_nothing());
    UT_AssertIsFalse(x);
    UT_AssertIsTrue(!x);
    UT_AssertThrows(x.value(), bond::CoreException);

    x.set_value() = 10;

    ConstInterface(x);

    UT_AssertIsFalse(x.is_nothing());
    UT_AssertIsTrue(x);
    UT_AssertIsFalse(!x);

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

TEST_CASE_BEGIN(CopyAndMoveFromValues)
{
    // BOOST_TEST_CONTEXT("simple type")
    {
        int i = 100;
        bond::maybe<int> copiedValue{ i };
        BOOST_REQUIRE(copiedValue);
        BOOST_CHECK(copiedValue.value() == 100);

        copiedValue = 200;
        BOOST_REQUIRE(copiedValue);
        BOOST_CHECK(copiedValue.value() == 200);
    }

    // BOOST_TEST_CONTEXT("move-only type")
    {
        std::unique_ptr<int> up{ new int{ 100 } };
        bond::maybe<std::unique_ptr<int>> movedFromValue{ std::move(up) };
        BOOST_CHECK(!up);
        BOOST_REQUIRE(movedFromValue);
        BOOST_CHECK(*movedFromValue.value() == 100);

        up.reset(new int{ 200 });
        movedFromValue = std::move(up);
        BOOST_CHECK(!up);
        BOOST_REQUIRE(movedFromValue);
        BOOST_CHECK(*movedFromValue.value() == 200);
    }

    // BOOST_TEST_CONTEXT("type with allocator")
    {
        auto numbers = { 1, 2, 3, 4, 5 };
        auto primes = { 2, 3, 5 };
        std::vector<int> vecNumbers{ std::begin(numbers), std::end(numbers) };
        std::vector<int> vecPrimes{ std::begin(primes), std::end(primes) };

        bond::maybe<std::vector<int>> copiedValue{ vecNumbers };
        BOOST_REQUIRE(copiedValue);
        BOOST_CHECK(copiedValue.value() == vecNumbers);

        copiedValue = vecPrimes;
        BOOST_REQUIRE(copiedValue);
        BOOST_CHECK(copiedValue.value() == vecPrimes);

        bond::maybe<std::vector<int>> movedValue{ std::move(vecNumbers) };
        BOOST_CHECK(vecNumbers.empty());
        BOOST_REQUIRE(movedValue);
        BOOST_CHECK(movedValue.value().size() == 5);

        movedValue = std::move(vecPrimes);
        BOOST_CHECK(vecPrimes.empty());
        BOOST_REQUIRE(movedValue);
        BOOST_CHECK(movedValue.value().size() == 3);
    }

    // BOOST_TEST_CONTEXT("move-only type with allocator")
    {
        std::vector<std::unique_ptr<int>> source1;
        source1.emplace_back(new int{ 1 });

        bond::maybe<std::vector<std::unique_ptr<int>>> movedValue{ std::move(source1) };
        BOOST_CHECK(source1.empty());
        BOOST_REQUIRE(movedValue);
        BOOST_CHECK(movedValue.value().size() == 1);

        std::vector<std::unique_ptr<int>> source2;
        source2.emplace_back(new int{ 2 });
        source2.emplace_back(new int{ 2 });

        movedValue = std::move(source2);
        BOOST_CHECK(source2.empty());
        BOOST_REQUIRE(movedValue);
        BOOST_CHECK(movedValue.value().size() == 2);
    }
}
TEST_CASE_END

TEST_CASE_BEGIN(MovedFromIsNothing)
{
    // BOOST_TEST_CONTEXT("simple type")
    {
        bond::maybe<bool> m{ false };
        BOOST_REQUIRE(!m.is_nothing());

        bond::maybe<bool> sink{ std::move(m) };
        BOOST_REQUIRE(m.is_nothing());
        BOOST_REQUIRE(!sink.is_nothing());
        BOOST_CHECK(!sink.value());
    }

    // BOOST_TEST_CONTEXT("move-only type")
    {
        bond::maybe<std::unique_ptr<int>> moveOnly;
        moveOnly.emplace(new int{ 42 });
        BOOST_REQUIRE(!moveOnly.is_nothing());

        bond::maybe<std::unique_ptr<int>> moveOnlySink{ std::move(moveOnly) };
        BOOST_REQUIRE(moveOnly.is_nothing());
        BOOST_REQUIRE(!moveOnlySink.is_nothing());
        BOOST_CHECK(*moveOnlySink.value() == 42);
    }

    // BOOST_TEST_CONTEXT("move-only type with allocator")
    {
        bond::maybe<std::vector<std::unique_ptr<int>>> m;
        m.emplace();
        m.value().emplace_back(new int{ 1 });
        m.value().emplace_back(new int{ 2 });
        BOOST_REQUIRE(!m.is_nothing());

        bond::maybe<std::vector<std::unique_ptr<int>>> sink{ std::move(m) };
        BOOST_REQUIRE(m.is_nothing());
        BOOST_REQUIRE(!sink.is_nothing());
        BOOST_CHECK(sink.value().size() == 2);
    }
}
TEST_CASE_END

struct UsesAllocator
{
    using allocator_type = allocator_with_state<>;

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

    UsesAllocator& operator=(UsesAllocator&&) = default;

    boost::optional<allocator_type> constructed_alloc;
    boost::optional<allocator_type> copied_alloc;
    boost::optional<allocator_type> moved_alloc;
};

TEST_CASE_BEGIN(AllocatorPropagated)
{
    allocator_with_state<> a1{ std::make_shared<int>() };
    allocator_with_state<> a2{ std::make_shared<int>() };

    bond::maybe<UsesAllocator> m{ a1 };
    m.set_value();
    BOOST_REQUIRE(!m.is_nothing());
    BOOST_CHECK(m.value().constructed_alloc == a1);
    BOOST_CHECK(!m.value().copied_alloc);
    BOOST_CHECK(!m.value().moved_alloc);

    // BOOST_TEST_CONTEXT("Copy")
    {
        bond::maybe<UsesAllocator> mCopy{ m };
        BOOST_REQUIRE(!mCopy.is_nothing());
        BOOST_CHECK(mCopy.value().constructed_alloc == a1);
        BOOST_CHECK(!mCopy.value().copied_alloc);
        BOOST_CHECK(!mCopy.value().moved_alloc);
    }

    // BOOST_TEST_CONTEXT("Copy & change allocator")
    {
        bond::maybe<UsesAllocator> mCopyAlloc{ m, a2 };
        BOOST_REQUIRE(!mCopyAlloc.is_nothing());
        BOOST_CHECK(!mCopyAlloc.value().constructed_alloc);
        BOOST_CHECK(mCopyAlloc.value().copied_alloc == a2);
        BOOST_CHECK(!mCopyAlloc.value().moved_alloc);
    }

    // BOOST_TEST_CONTEXT("Move")
    {
        bond::maybe<UsesAllocator> mMove{ std::move(m) };
        BOOST_REQUIRE(!mMove.is_nothing());
        BOOST_CHECK(mMove.value().constructed_alloc == a1);
        BOOST_CHECK(!mMove.value().copied_alloc);
        BOOST_CHECK(!mMove.value().moved_alloc);
    }

    // We just moved from m, so we need to re-create it for the next test.
    m = bond::maybe<UsesAllocator>{ a1 };
    m.set_value();
    BOOST_REQUIRE(!m.is_nothing());
    BOOST_CHECK(m.value().constructed_alloc == a1);
    BOOST_CHECK(!m.value().copied_alloc);
    BOOST_CHECK(!m.value().moved_alloc);

    // BOOST_TEST_CONTEXT("Move & change allocator")
    {
        bond::maybe<UsesAllocator> mMoveAlloc{ std::move(m), a2 };
        BOOST_REQUIRE(!mMoveAlloc.is_nothing());
        BOOST_CHECK(!mMoveAlloc.value().constructed_alloc);
        BOOST_CHECK(!mMoveAlloc.value().copied_alloc);
        BOOST_CHECK(mMoveAlloc.value().moved_alloc == a2);
    }

    // BOOST_TEST_CONTEXT("implicitly create")
    {
        bond::maybe<UsesAllocator> hasAlloc{ a1 };
        hasAlloc.set_value();
        BOOST_REQUIRE(!hasAlloc.is_nothing());
        BOOST_CHECK(hasAlloc.value().constructed_alloc == a1);
        BOOST_CHECK(!hasAlloc.value().copied_alloc);
        BOOST_CHECK(!hasAlloc.value().moved_alloc);
    }

    // BOOST_TEST_CONTEXT("explicitly create with different allocator")
    {
        bond::maybe<UsesAllocator> hasAlloc{ a1 };
        hasAlloc.emplace(a2);
        BOOST_REQUIRE(!hasAlloc.is_nothing());
        BOOST_CHECK(hasAlloc.value().constructed_alloc == a2);
        BOOST_CHECK(!hasAlloc.value().copied_alloc);
        BOOST_CHECK(!hasAlloc.value().moved_alloc);
    }
}
TEST_CASE_END

BOOST_STATIC_ASSERT(std::is_same<int, bond::maybe<int>::value_type>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_default_constructible<bond::maybe<int>>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_constructible<bond::maybe<int>>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_assignable<bond::maybe<int>>::value);

BOOST_STATIC_ASSERT(std::is_same<std::vector<int>, bond::maybe<std::vector<int>>::value_type>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_default_constructible<bond::maybe<std::vector<int>>>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_constructible<bond::maybe<std::vector<int>>>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_assignable<bond::maybe<std::vector<int>>>::value);

// UsesAllocator uses allocator_with_state as its allocator, so
// bond::maybe<UsesAllocator> cannot be default constructued, as bond::maybe
// needs to be able to default construct the allocator to be default
// constructible.
BOOST_STATIC_ASSERT(std::is_same<UsesAllocator, bond::maybe<UsesAllocator>::value_type>::value);
BOOST_STATIC_ASSERT(!std::is_default_constructible<bond::maybe<UsesAllocator>>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_constructible<bond::maybe<UsesAllocator>>::value);
BOOST_STATIC_ASSERT(std::is_nothrow_move_assignable<bond::maybe<UsesAllocator>>::value);

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
    AddTestCase<TEST_ID(0x1a06), CopyAndMoveFromValues>(suite, "APIs");
    AddTestCase<TEST_ID(0x1a07), MovedFromIsNothing>(suite, "APIs");
    AddTestCase<TEST_ID(0x1a08), AllocatorPropagated>(suite, "APIs");
}


bool init_unit_test()
{
    MaybeTest::Initialize();
    return true;
}
