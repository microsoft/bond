#pragma once


template <typename Protocols>
inline bool Compare(const SimpleStruct& left, const SimpleStructView& right)
{
    return Compare<Protocols>(left.m_bool, right.m_bool)
        && Compare<Protocols>(left.m_str, right.m_str)
        && Compare<Protocols>(left.m_int8, right.m_int8)
        && Compare<Protocols>(left.m_uint64, right.m_uint64)
        && Compare<Protocols>(left.m_float, right.m_float)
        && Compare<Protocols>(left.m_enum1, right.m_enum1);
}

template <typename Protocols>
inline bool Compare(const NestedStruct& left, const NestedStructView& right)
{
    return Compare<Protocols>(left.m_int8, right.m_int8)
        && Compare<Protocols>(left.m_int16, right.m_int16)
        && Equal<Protocols>(left.n1, right.n1);
}

template <typename Protocols>
inline bool Compare(const StructWithBase& left, const SimpleStruct& right)
{
    return Equal<Protocols>(static_cast<SimpleStruct>(left), right);
}

template <typename Protocols>
inline bool Compare(const StructWithBase& left, const SimpleBase& right)
{
    return Equal<Protocols>(static_cast<SimpleBase>(left), right);
}

template <typename Protocols>
inline bool Compare(const SimpleBase& left, const SimpleBaseView& right)
{
    return Compare<Protocols>(left.m_int32, right.m_int32)
        && Compare<Protocols>(left.m_enum1, right.m_enum1)
        && BaseIsEqual<Protocols>(left, right);
}

template <typename Protocols>
inline bool Compare(const StructWithBase& left, const StructWithBaseView& right)
{
    return Compare<Protocols>(left.m_str, right.m_str)
        && Compare<Protocols>(left.m_uint32, right.m_uint32)
        && BaseIsEqual<Protocols>(left, right);
}

template <typename Protocols>
inline bool Compare(const NestedWithBase& left, const NestedWithBaseView& right)
{
    return Equal<Protocols>(left.n2, right.n2)
        && Equal<Protocols>(left.d, right.d)
        && BaseIsEqual<Protocols>(left, right);
}

template <typename Protocols>
inline bool Compare(const NestedWithBase& left, const NestedWithBase2& right)
{
    return Equal<Protocols>(static_cast<NestedWithBase2>(left), right);
}

template <typename Protocols>
inline bool Compare(const ListWithBase& left, const ListWithBaseView& right)
{
    return Equal<Protocols>(left.l1, right.l1)
        && Equal<Protocols>(left.v3, right.v3);
}

template <typename Protocols, typename NestedStruct1>
inline bool Compare(const NestedStruct1& left, const NestedStruct1OptionalBondedView& right)
{
    return Equal<Protocols>(left.s, right.s);
}


class SerializationTest
{
public:
    static void Initialize()
    {
        SimpleStructTestsInit();
    }

private:
    static void SimpleStructTestsInit();
};

