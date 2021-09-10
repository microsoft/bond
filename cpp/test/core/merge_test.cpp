// Required by a particular set of tests. The affected tests have comments
// referencing this macro.
#define BOND_UNIT_TEST_ONLY_PERMIT_OBJECT_REUSE

#include "precompiled.h"
#include "serialization_test.h"


template <typename Reader, typename Writer, typename Protocols = bond::BuiltInProtocols, typename Payload, typename T>
void Merging(Payload payload, const T& obj, uint16_t version = bond::v1, bool mergeByDeserialize = true);


template <typename Reader, typename Writer, typename Payload, typename T, typename Protocols = bond::BuiltInProtocols>
void MergingRandom()
{
    // random values
    for (uint32_t i = 0; i < c_iterations; ++i)
    {
        Merging<Reader, Writer, Protocols>(InitRandom<Payload, Protocols>(), InitRandom<T, Protocols>());
        Merging<Reader, Writer, Protocols>(InitRandom<Payload, Protocols>(), InitRandom<T, Protocols>(), Reader::version);
    }
}


template <typename Reader, typename Writer, typename Payload, typename T, typename Protocols = bond::BuiltInProtocols>
void AllMerging()
{
    // default value
    Merging<Reader, Writer, Protocols>(Payload(), T());
    Merging<Reader, Writer, Protocols>(Payload(), T(), Reader::version);
    Merging<Reader, Writer, Protocols>(InitRandom<Payload, Protocols>(), T());
    Merging<Reader, Writer, Protocols>(InitRandom<Payload, Protocols>(), T(), Reader::version);
    Merging<Reader, Writer, Protocols>(Payload(), InitRandom<T, Protocols>());
    Merging<Reader, Writer, Protocols>(Payload(), InitRandom<T, Protocols>(), Reader::version);
    
    // random values
    MergingRandom<Reader, Writer, Payload, T, Protocols>();
}


namespace unittest
{

template <typename Protocols, typename T1, typename T2>
typename boost::enable_if<bond::is_container<T1>, bool>::type 
MergedEqual(const T1& payload, const T1& merged, const T2& obj);

template <typename Protocols, typename T1, typename T2>
typename boost::disable_if<bond::is_container<T1>, bool>::type 
MergedEqual(const T1&, const T1&, const T2&)
{
    return false;
}

template <typename Protocols, typename P, typename T1, typename T2, typename T3, typename T4>
bool MergedEqual(bond::const_enumerator<P>&, const P& map, const std::pair<T1, T2>& p1, const std::pair<T3, T4>& p2)
{
    if (!Equal<Protocols>(p1.first, p2.first))
        return false;

    if (map.end() != map.find(p1.first))
        return MergedEqual<Protocols>(map.find(p1.first)->second, p1.second, p2.second);
    else
        return MergedEqual<Protocols>(typename bond::element_type<P>::type::second_type(), p1.second, p2.second);
}


template <typename Protocols, typename P, typename T1, typename T2>
bool MergedEqual(bond::const_enumerator<P>& items, const P&, const T1& i1, const T2& i2)
{
    if (items.more())
        return MergedEqual<Protocols>(items.next(), i1, i2);
    else
        return MergedEqual<Protocols>(typename bond::element_type<P>::type(), i1, i2);
}

template <typename Protocols, typename T1, typename T2>
typename boost::enable_if<bond::is_container<T1>, bool>::type 
MergedEqual(const T1& payload, const T1& merged, const T2& obj)
{
    if (container_size(obj) != container_size(merged))
        return false;

    bond::const_enumerator<T1> payload_items(payload);
    bond::const_enumerator<T1> merged_items(merged);
    bond::const_enumerator<T2> obj_items(obj);

    while (merged_items.more())
        if (!MergedEqual<Protocols>(payload_items, payload, merged_items.next(), obj_items.next()))
            return false;

    return true;
}

template <typename Protocols>
bool MergedEqual(const SimpleListsStruct& payload, const SimpleListsStruct& merged, const SimpleListsStructView& obj)
{
    return Compare<Protocols>(merged.l_bool, obj.l_bool)
        && Compare<Protocols>(merged.l_int64, obj.l_int64)
        && Compare<Protocols>(merged.l_float, obj.l_float)
        && Compare<Protocols>(merged.l_string, obj.l_string)
        && Compare<Protocols>(merged.v_int16, obj.v_int16)
        && Compare<Protocols>(merged.v_double, obj.v_double)
        && Compare<Protocols>(merged.v_string, obj.v_string)
        && Compare<Protocols>(merged.s_uint64, obj.s_uint64)
        && Compare<Protocols>(merged.s_string, obj.s_string)
        && Compare<Protocols>(merged.m_int8_string, obj.m_int8_string)
        && Compare<Protocols>(merged.m_string_bool, obj.m_string_bool)

        && Compare<Protocols>(merged.l_uint32, payload.l_uint32)
        && Compare<Protocols>(merged.l_int16, payload.l_int16)
        && Compare<Protocols>(merged.v_bool, payload.v_bool)
        && Compare<Protocols>(merged.v_uint8, payload.v_uint8)
        && Compare<Protocols>(merged.v_int64, payload.v_int64)
        && Compare<Protocols>(merged.s_bool, payload.s_bool)
        && Compare<Protocols>(merged.s_float, payload.s_float)
        && Compare<Protocols>(merged.m_float_uint16, payload.m_float_uint16);
}

template <typename Protocols>
bool MergedEqual(const NestedStruct& payload, const NestedStruct& merged, const NestedStructView& obj)
{
    return Compare<Protocols>(merged.m_int8, obj.m_int8)
        && Compare<Protocols>(merged.n1, obj.n1)
        && Compare<Protocols>(merged.m_int16, obj.m_int16)

        && Compare<Protocols>(merged.n3, payload.n3)
        && Compare<Protocols>(merged.m_bool, payload.m_bool)
        && Compare<Protocols>(merged.n2, payload.n2)
        && Compare<Protocols>(merged.m_int32, payload.m_int32)
        && Compare<Protocols>(merged.m_int64, payload.m_int64)
        && Compare<Protocols>(merged.m_uint8, payload.m_uint8)
        && Compare<Protocols>(merged.m_uint16, payload.m_uint16)
        && Compare<Protocols>(merged.m_uint32, payload.m_uint32)
        && Compare<Protocols>(merged.m_uint64, payload.m_uint64)
        && Compare<Protocols>(merged.m_double, payload.m_double)
        && Compare<Protocols>(merged.m_float, payload.m_float)
        && Compare<Protocols>(merged.m_enum1, payload.m_enum1)
        && Compare<Protocols>(merged.m_str, payload.m_str);
}

template <typename Protocols>
bool MergedEqual(const NestedMaps& payload, const NestedMaps& merged, const NestedMapsView& obj)
{
    return Equal<Protocols>(payload.m64ls, merged.m64ls)

        && MergedEqual<Protocols>(payload.msSLS, merged.msSLS, obj.msSLS)
        && MergedEqual<Protocols>(payload.m32lNS, merged.m32lNS, obj.m32lNS);
}

template <typename Protocols>
bool MergedEqual(const NestedListsStruct& payload, const NestedListsStruct& merged, const NestedListsView& obj)
{
    return Equal<Protocols>(payload.ll8, merged.ll8)
        && Equal<Protocols>(payload.lvls, merged.lvls)
        && Equal<Protocols>(payload.SLS, merged.SLS)
        && Equal<Protocols>(payload.vf, merged.vf)
        && Equal<Protocols>(payload.vss, merged.vss)
        && Equal<Protocols>(payload.lsb, merged.lsb)
        && Equal<Protocols>(payload.m64ls, merged.m64ls)
        && Equal<Protocols>(payload.vmds, merged.vmds)

        && MergedEqual<Protocols>(payload.lSLS, merged.lSLS, obj.lSLS)
        && MergedEqual<Protocols>(payload.vlSLS, merged.vlSLS, obj.vlSLS)
        && MergedEqual<Protocols>(payload.vvNS, merged.vvNS, obj.vvNS);
}

} // namespace unittest

template <typename Reader, typename Writer, typename Protocols, typename Payload, typename T>
void Merging(Payload payload, const T& obj, uint16_t version, bool mergeByDeserialize)
{
    Reader merged = Merge<Reader, Writer, Protocols>(payload, obj, version);

    // Deserialize merged into T and compare against obj
    {
        T to;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127) // C4127: conditional expression is constant
#endif
        if (boost::mpl::count_if<typename T::Schema::fields, is_optional_field<_> >::value == 0)
#ifdef _MSC_VER
#pragma warning(pop)
#endif

        {
            to = InitRandom<T, Protocols>();
            Fixup(to);
        }

        bond::Deserialize<Protocols>(merged, to);

        UT_Equal_P(obj, to, Protocols);
    }

    // Deserialize merged into Payload and compare against combination of the 
    // orginal payload and the obj.
    {
        Payload to;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127) // C4127: conditional expression is constant
#endif
        if (boost::mpl::count_if<typename Payload::Schema::fields, is_optional_field<_> >::value == 0)
#ifdef _MSC_VER
#pragma warning(pop)
#endif
        {
            to = InitRandom<Payload, Protocols>();
            Fixup(to);
        }
        
        bond::Deserialize<Protocols>(merged, to);

        if (mergeByDeserialize)
        {
            // Will fail an assert without
            // #define BOND_UNIT_TEST_ONLY_PERMIT_OBJECT_REUSE
            bond::Deserialize<Protocols>(Serialize<Reader, Writer, Protocols>(obj, version), payload);

            UT_Equal_P(payload, to, Protocols);
        }
        else
        {
            UT_AssertIsTrue(unittest::MergedEqual<Protocols>(payload, to, obj));
        }
    }
}


class ModifyContainers
    : public bond::ModifyingTransform
{
public:
    void Begin(const bond::Metadata&) const
    {}

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(T& base) const
    {
        return false;
    }

    template <typename T>
    typename boost::enable_if<bond::is_container<T>, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& /*metadata*/, T& field) const
    {
        Modify(field);
        return false;
    }

    template <typename T>
    typename boost::disable_if<bond::is_container<T>, bool>::type
    Field(uint16_t /*id*/, const bond::Metadata& metadata, T& field) const
    {
        return false;
    }

private:
    template <typename T1, typename T2>
    void Modify(std::pair<T1, T2>& pair) const
    {
        Modify(pair.second);
    }


    template <typename T>
    typename boost::enable_if<bond::is_container<T> >::type
    Modify(T& x) const
    {
        for (bond::enumerator<T> items(x); items.more();)
            Modify(items.next());
    }


    template <typename T>
    typename boost::disable_if<bond::is_container<T> >::type
    Modify(T& x) const
    {
        x = InitRandom<T>();
    }
};


template <typename Reader, typename Writer, typename T1, typename T2>
void ModifyAndMergeContainers(const ModifyContainers& transform)
{
    T1 payload = InitRandom<T1>();

    T2 obj;
    
    bond::Deserialize(Serialize<Reader, Writer>(payload), obj);

    Apply(transform, obj);

    Merging<Reader, Writer>(payload, obj, bond::v1, false);
    Merging<Reader, Writer>(payload, obj, Reader::version, false);
}


template <typename Reader, typename Writer, typename T1, typename T2>
TEST_CASE_BEGIN(MergingContainers)
{
    ModifyAndMergeContainers<Reader, Writer, T1, T2>(ModifyContainers());
}
TEST_CASE_END


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(MergingOptionals)
{
    SimpleOptionals payload;
    payload.a = 'a';
    payload.c = 'c';

    SimpleOptionalsView obj;
    obj.b = 'b';
    
    Reader merged = Merge<Reader, Writer>(payload, obj);

    payload.b = obj.b;

    SimpleOptionals merged_payload;
    bond::Deserialize(merged, merged_payload);
    UT_Equal(merged_payload, payload);

    SimpleOptionalsView merged_obj;
    bond::Deserialize(merged, merged_obj);
    UT_Equal(merged_obj, obj);
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T1, typename T2>
TEST_CASE_BEGIN(MergingAll)
{
    AllMerging<Reader, Writer, T1, T2>();
    AllMerging<Reader, Writer, T2, T1>();
}
TEST_CASE_END


template <typename Reader, typename Writer, typename T1, typename T2>
TEST_CASE_BEGIN(MergingInitialized)
{
    MergingRandom<Reader, Writer, T1, T2>();
    MergingRandom<Reader, Writer, T2, T1>();
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void MergeTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N),
        MergingOptionals, Reader, Writer>(suite, "Merging simple struct with optionals");

    AddTestCase<TEST_ID(N), 
        MergingAll, Reader, Writer, NestedStructView, NestedStruct>(suite, "Merging nested struct");

    AddTestCase<TEST_ID(N), 
        MergingInitialized, Reader, Writer, NestedStructBondedView, NestedStruct>(suite, "Merging bonded nested struct");

    AddTestCase<TEST_ID(N), 
        MergingAll, Reader, Writer, NestedWithBaseView, NestedWithBase>(suite, "Merging struct with base");

    AddTestCase<TEST_ID(N), 
        MergingAll, Reader, Writer, SimpleListsStructView, SimpleListsStruct>(suite, "Merging simple containers");

    AddTestCase<TEST_ID(N), 
        MergingAll, Reader, Writer, NestedListsStructBondedViewView, NestedListsStructBondedView>(suite, "Merging bonded containers");

    AddTestCase<TEST_ID(N), 
        MergingContainers, Reader, Writer, NestedListsStruct, NestedListsView>(suite, "Merging struct lists");

    AddTestCase<TEST_ID(N), 
        MergingContainers, Reader, Writer, NestedMaps, NestedMapsView>(suite, "Merging struct maps");
}


void MergeTestsInit()
{
    TEST_COMPACT_BINARY_PROTOCOL(
        MergeTests<
            0x2302,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Merge tests for CompactBinary");
    );

    TEST_FAST_BINARY_PROTOCOL(
        MergeTests<
            0x2303,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Merge tests for FastBinary");
    );
}

bool init_unit_test()
{
    MergeTestsInit();
    return true;
}

