

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

namespace nsmapped
{
    using System.Collections.Generic;

    [global::Bond.Namespace("tests")]
    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class BasicTypes
    {
        [global::Bond.Id(0)]
        public bool _bool { get; set; }

        [global::Bond.Id(2)]
        public string _str { get; set; }

        [global::Bond.Id(3), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string _wstr { get; set; }

        [global::Bond.Id(10)]
        public ulong _uint64 { get; set; }

        [global::Bond.Id(11)]
        public ushort _uint16 { get; set; }

        [global::Bond.Id(12)]
        public uint _uint32 { get; set; }

        [global::Bond.Id(13)]
        public byte _uint8 { get; set; }

        [global::Bond.Id(14)]
        public sbyte _int8 { get; set; }

        [global::Bond.Id(15)]
        public short _int16 { get; set; }

        [global::Bond.Id(16)]
        public int _int32 { get; set; }

        [global::Bond.Id(17)]
        public long _int64 { get; set; }

        [global::Bond.Id(18)]
        public double _double { get; set; }

        [global::Bond.Id(20)]
        public float _float { get; set; }

        [global::Bond.Id(21)]
        public System.ArraySegment<byte> _blob { get; set; }

        public BasicTypes()
            : this("tests.BasicTypes", "BasicTypes")
        {}

        protected BasicTypes(string fullName, string name)
        {
            _str = "";
            _wstr = "";
            _blob = new System.ArraySegment<byte>();
        }
    }
} // nsmapped
