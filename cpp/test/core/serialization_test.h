#pragma once

namespace unittest
{

inline bool operator==(const SimpleStruct& left, const SimpleStructView& right)
{
    return left.m_bool == right.m_bool
        && left.m_str == right.m_str
        && left.m_int8 == right.m_int8
        && left.m_uint64 == right.m_uint64
        && left.m_float == right.m_float
        && left.m_enum1 == right.m_enum1;
}


inline bool operator==(const NestedStruct& left, const NestedStructView& right)
{
    return left.m_int8 == right.m_int8
        && left.m_int16 == right.m_int16
        && Equal(left.n1, right.n1);
}


inline bool operator==(const StructWithBase& left, const SimpleStruct& right)
{
    return Equal(static_cast<SimpleStruct>(left), right);
}


inline bool operator==(const StructWithBase& left, const SimpleBase& right)
{
    return Equal(static_cast<SimpleBase>(left), right);
}


inline bool operator==(const SimpleBase& left, const SimpleBaseView& right)
{
    return left.m_int32 == right.m_int32
        && left.m_enum1 == right.m_enum1
        && BaseIsEqual(left, right);
}


inline bool operator==(const StructWithBase& left, const StructWithBaseView& right)
{
    return left.m_str == right.m_str
        && left.m_uint32 == right.m_uint32
        && BaseIsEqual(left, right);
}


inline bool operator==(const NestedWithBase& left, const NestedWithBaseView& right)
{
    return Equal(left.n2, right.n2)
        && Equal(left.d, right.d)
        && BaseIsEqual(left, right);
}


inline bool operator==(const NestedWithBase& left, const NestedWithBase2& right)
{
    return Equal(static_cast<NestedWithBase2>(left), right);
}


inline bool operator==(const ListWithBase& left, const ListWithBaseView& right)
{
    return Equal(left.l1, right.l1)
        && Equal(left.v3, right.v3);
}


template <typename NestedStruct1>
inline bool operator==(const NestedStruct1& left, const NestedStruct1OptionalBondedView& right)
{
    return Equal(left.s, right.s);
}

} // namespace unittest

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

