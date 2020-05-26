

// suppress "Missing XML comment for publicly visible type or member"
#pragma warning disable 1591


#region ReSharper warnings
// ReSharper disable PartialTypeWithSinglePart
// ReSharper disable RedundantNameQualifier
// ReSharper disable InconsistentNaming
// ReSharper disable CheckNamespace
// ReSharper disable UnusedParameter.Local
// ReSharper disable RedundantUsingDirective
#endregion

namespace tests
{
    using System.Collections.Generic;

    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public enum EnumType1
    {
        EnumValue1 = unchecked((int)5),
        EnumValue2 = unchecked((int)10),
        EnumValue3 = unchecked((int)-10),
        EnumValue4 = unchecked((int)42),
        Low = unchecked((int)1),
        EnumValue5 = unchecked((int)-10),
        EnumValue6 = unchecked((int)4294967286),
        Int32Min = unchecked((int)-2147483648),
        Int32Max = unchecked((int)2147483647),
        UInt32Min = unchecked((int)0),
        UInt32Max = unchecked((int)4294967295),
        HexNeg = unchecked((int)-255),
        OctNeg = unchecked((int)-83),
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Foo
    {
        [global::Bond.Id(0)]
        public bool m_bool_1 { get; set; }

        [global::Bond.Id(1)]
        public bool m_bool_2 { get; set; }

        [global::Bond.Id(2)]
        public bool? m_bool_3 { get; set; }

        [global::Bond.Id(3)]
        public string m_str_1 { get; set; }

        [global::Bond.Id(4)]
        public string m_str_2 { get; set; }

        [global::Bond.Id(5)]
        public sbyte m_int8_4 { get; set; }

        [global::Bond.Id(6)]
        public sbyte? m_int8_5 { get; set; }

        [global::Bond.Id(7)]
        public short m_int16_4 { get; set; }

        [global::Bond.Id(8)]
        public short? m_int16_5 { get; set; }

        [global::Bond.Id(9)]
        public int? m_int32_4 { get; set; }

        [global::Bond.Id(10)]
        public int m_int32_max { get; set; }

        [global::Bond.Id(11)]
        public long? m_int64_4 { get; set; }

        [global::Bond.Id(12)]
        public long m_int64_max { get; set; }

        [global::Bond.Id(13)]
        public byte m_uint8_2 { get; set; }

        [global::Bond.Id(14)]
        public byte? m_uint8_3 { get; set; }

        [global::Bond.Id(15)]
        public ushort m_uint16_2 { get; set; }

        [global::Bond.Id(16)]
        public ushort? m_uint16_3 { get; set; }

        [global::Bond.Id(17)]
        public uint? m_uint32_3 { get; set; }

        [global::Bond.Id(18)]
        public uint m_uint32_max { get; set; }

        [global::Bond.Id(19)]
        public ulong? m_uint64_3 { get; set; }

        [global::Bond.Id(20)]
        public ulong m_uint64_max { get; set; }

        [global::Bond.Id(21)]
        public double? m_double_3 { get; set; }

        [global::Bond.Id(22)]
        public double m_double_4 { get; set; }

        [global::Bond.Id(23)]
        public double m_double_5 { get; set; }

        [global::Bond.Id(24)]
        public float? m_float_3 { get; set; }

        [global::Bond.Id(25)]
        public float m_float_4 { get; set; }

        [global::Bond.Id(26)]
        public float m_float_7 { get; set; }

        [global::Bond.Id(27)]
        public EnumType1 m_enum1 { get; set; }

        [global::Bond.Id(28)]
        public EnumType1 m_enum2 { get; set; }

        [global::Bond.Id(29)]
        public EnumType1? m_enum3 { get; set; }

        [global::Bond.Id(30)]
        public EnumType1 m_enum_int32min { get; set; }

        [global::Bond.Id(31)]
        public EnumType1 m_enum_int32max { get; set; }

        [global::Bond.Id(32)]
        public EnumType1 m_enum_uint32_min { get; set; }

        [global::Bond.Id(33)]
        public EnumType1 m_enum_uint32_max { get; set; }

        [global::Bond.Id(34), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string m_wstr_1 { get; set; }

        [global::Bond.Id(35), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string m_wstr_2 { get; set; }

        [global::Bond.Id(36)]
        public long m_int64_neg_hex { get; set; }

        [global::Bond.Id(37)]
        public long m_int64_neg_oct { get; set; }

        public Foo()
            : this("tests.Foo", "Foo")
        {}

        protected Foo(string fullName, string name)
        {
            m_bool_1 = true;
            m_bool_2 = false;
            m_str_1 = "default string value";
            m_int8_4 = -127;
            m_int16_4 = -32767;
            m_int32_max = 2147483647;
            m_int64_max = 9223372036854775807;
            m_uint8_2 = 255;
            m_uint16_2 = 65535;
            m_uint32_max = 4294967295;
            m_uint64_max = 18446744073709551615;
            m_double_4 = -123.456789;
            m_double_5 = -0.0;
            m_float_4 = 2.71828183F;
            m_float_7 = 0.0F;
            m_enum1 = EnumType1.EnumValue1;
            m_enum2 = EnumType1.EnumValue3;
            m_enum_int32min = EnumType1.Int32Min;
            m_enum_int32max = EnumType1.Int32Max;
            m_enum_uint32_min = EnumType1.UInt32Min;
            m_enum_uint32_max = EnumType1.UInt32Max;
            m_wstr_1 = "default wstring value";
            m_int64_neg_hex = -4095;
            m_int64_neg_oct = -83;
        }
    }
} // tests
