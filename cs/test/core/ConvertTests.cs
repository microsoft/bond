namespace UnitTest.Convert
{
    using System;
    using System.Collections.Generic;
    using System.Text;
    using System.Threading;
    using Bond;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;

    [global::Bond.Schema]
    public class Dates
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(long))]
        public System.DateTime _date { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(List<long>))]
        public List<System.DateTime> _dateVector { get; set; }

        [global::Bond.Id(2), global::Bond.Type(typeof(LinkedList<long>))]
        public LinkedList<System.DateTime> _dateList { get; set; }

        [global::Bond.Id(3), global::Bond.Type(typeof(Dictionary<int,long>))]
        public Dictionary<int, System.DateTime> _dateMap { get; set; }

        [global::Bond.Id(4), global::Bond.Type(typeof(global::Bond.Tag.nullable<long>))]
        public System.DateTime? _dateNullable { get; set; }

        [global::Bond.Id(5), global::Bond.Type(typeof(long)), global::Bond.Required]
        public System.DateTime _dateRequired { get; set; }

        [global::Bond.Id(6), global::Bond.Type(typeof(long)), global::Bond.RequiredOptional]
        public System.DateTime _dateRequiredOptional { get; set; }

        public Dates()
            : this("UnitTest.Convert.Dates", "Dates")
        { }

        protected Dates(string fullName, string name)
        {
            _date = new System.DateTime();
            _dateVector = new List<System.DateTime>();
            _dateList = new LinkedList<System.DateTime>();
            _dateMap = new Dictionary<int, System.DateTime>();
            _dateNullable = null;
            _dateRequired = new System.DateTime();
            _dateRequiredOptional = new System.DateTime();
        }

        internal int CountInstances()
        {
            return 3 + _dateVector.Count + _dateList.Count + _dateMap.Count + (_dateNullable.HasValue ? 1 : 0);
        }
    }

    [global::Bond.Schema]
    public class Decimals
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.blob))]
        public decimal _dec { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(List<global::Bond.Tag.blob>))]
        public List<decimal> _decVector { get; set; }

        [global::Bond.Id(2), global::Bond.Type(typeof(LinkedList<global::Bond.Tag.blob>))]
        public LinkedList<decimal> _decList { get; set; }

        [global::Bond.Id(3), global::Bond.Type(typeof(Dictionary<int, global::Bond.Tag.blob>))]
        public Dictionary<int, decimal> _decMap { get; set; }

        [global::Bond.Id(4), global::Bond.Type(typeof(global::Bond.Tag.nullable<global::Bond.Tag.blob>))]
        public decimal? _decNullable { get; set; }

        [global::Bond.Id(5), global::Bond.Type(typeof(global::Bond.Tag.blob)), global::Bond.Required]
        public decimal _decRequired { get; set; }

        [global::Bond.Id(6), global::Bond.Type(typeof(global::Bond.Tag.blob)), global::Bond.RequiredOptional]
        public decimal _decRequiredOptional { get; set; }

        public Decimals()
            : this("UnitTest.Convert.Decimals", "Decimals")
        { }

        protected Decimals(string fullName, string name)
        {
            _dec = new decimal();
            _decVector = new List<decimal>();
            _decList = new LinkedList<decimal>();
            _decMap = new Dictionary<int, decimal>();
            _decNullable = null;
            _decRequired = new decimal();
            _decRequiredOptional = new decimal();
        }

        internal int CountInstances()
        {
            return 3 + _decVector.Count + _decList.Count + _decMap.Count + (_decNullable.HasValue ? 1 : 0);
        }
    }

    public class RefObject : IEquatable<RefObject>
    {
        public RefObject()
            : this("")
        { }

        public RefObject(string value)
        {
            Value = value;
        }

        public string Value { get; }

        public bool Equals(RefObject other)
        {
            if (ReferenceEquals(other, null))
                return false;
            if (ReferenceEquals(other, this))
                return true;

            return this.Value == other.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
        public override bool Equals(object obj)
        {
            return Equals(obj as RefObject);
        }

        public static bool operator ==(RefObject left, RefObject right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(RefObject left, RefObject right)
        {
            return !(left == right);
        }
    }

    [global::Bond.Schema]
    public class RefObjects
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.blob))]
        public RefObject _ref { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(List<global::Bond.Tag.blob>))]
        public List<RefObject> _refVector { get; set; }

        [global::Bond.Id(2), global::Bond.Type(typeof(LinkedList<global::Bond.Tag.blob>))]
        public LinkedList<RefObject> _refList { get; set; }

        [global::Bond.Id(3), global::Bond.Type(typeof(Dictionary<int, global::Bond.Tag.blob>))]
        public Dictionary<int, RefObject> _refMap { get; set; }

        [global::Bond.Id(4), global::Bond.Type(typeof(global::Bond.Tag.nullable<global::Bond.Tag.blob>))]
        public RefObject _refNullable { get; set; }

        [global::Bond.Id(5), global::Bond.Type(typeof(global::Bond.Tag.blob)), global::Bond.Required]
        public RefObject _refRequired { get; set; }

        [global::Bond.Id(6), global::Bond.Type(typeof(global::Bond.Tag.blob)), global::Bond.RequiredOptional]
        public RefObject _refRequiredOptional { get; set; }

        public RefObjects()
            : this("UnitTest.Convert.RefObjects", "RefObjects")
        { }

        protected RefObjects(string fullName, string name)
        {
            _ref = new RefObject();
            _refVector = new List<RefObject>();
            _refList = new LinkedList<RefObject>();
            _refMap = new Dictionary<int, RefObject>();
            _refNullable = null;
            _refRequired = new RefObject();
            _refRequiredOptional = new RefObject();
        }

        internal int CountInstances()
        {
            return 3 + _refVector.Count + _refList.Count + _refMap.Count + (_refNullable != null ? 1 : 0);
        }
    }

    public static class BondTypeAliasConverter
    {
        public static int ConvertToDateTimeCount = 0;
        public static int ConvertFromDateTimeCount = 0;

        public static int ConvertToDecimalCount = 0;
        public static int ConvertFromDecimalCount = 0;

        public static int ConvertToRefObjectCount = 0;
        public static int ConvertFromRefObjectCount = 0;

        public static DateTime Convert(long value, DateTime unused)
        {
            Interlocked.Increment(ref ConvertToDateTimeCount);
            return new DateTime(value, DateTimeKind.Utc);
        }

        public static long Convert(DateTime value, long unused)
        {
            Interlocked.Increment(ref ConvertFromDateTimeCount);
            return value.ToUniversalTime().Ticks;
        }

        public static decimal Convert(ArraySegment<byte> value, decimal unused)
        {
            var bits = new int[value.Count / sizeof(int)];
            Buffer.BlockCopy(value.Array, value.Offset, bits, 0, bits.Length * sizeof(int));

            Interlocked.Increment(ref ConvertToDecimalCount);

            return new decimal(bits);
        }

        public static ArraySegment<byte> Convert(decimal value, ArraySegment<byte> unused)
        {
            var bits = decimal.GetBits(value);
            var data = new byte[bits.Length * sizeof(int)];
            Buffer.BlockCopy(bits, 0, data, 0, data.Length);

            Interlocked.Increment(ref ConvertFromDecimalCount);

            return new ArraySegment<byte>(data);
        }

        public static RefObject Convert(ArraySegment<byte> value, RefObject unused)
        {
            Interlocked.Increment(ref ConvertToRefObjectCount);

            return new RefObject(Encoding.ASCII.GetString(value.Array, value.Offset, value.Count));
        }

        public static ArraySegment<byte> Convert(RefObject value, ArraySegment<byte> unused)
        {
            Interlocked.Increment(ref ConvertFromRefObjectCount);

            return new ArraySegment<byte>(Encoding.ASCII.GetBytes(value.Value));
        }

        public static void ResetCounts()
        {
            ConvertToDateTimeCount = 0;
            ConvertFromDateTimeCount = 0;

            ConvertToDecimalCount = 0;
            ConvertFromDecimalCount = 0;

            ConvertToRefObjectCount = 0;
            ConvertFromRefObjectCount = 0;
        }
    }


    [TestFixture]
    public class ConvertTests
    {
        [SetUp]
        public void Setup()
        {
            BondTypeAliasConverter.ResetCounts();
        }

        [Test]
        public void CorrectSerializeConvertCountLongStruct()
        {
            var foo = new Dates();
            foo._date = MakeUtcDateTime(2015,1,8);
            foo._dateVector.Add(MakeUtcDateTime(2015,1,9));
            foo._dateVector.Add(MakeUtcDateTime(2015,1,10));
            foo._dateList.AddLast(MakeUtcDateTime(2015, 1, 11));
            foo._dateList.AddLast(MakeUtcDateTime(2015, 1, 12));
            foo._dateList.AddLast(MakeUtcDateTime(2015, 1, 13));

            foo._dateMap.Add(1, MakeUtcDateTime(2015, 1, 14));
            foo._dateMap.Add(2, MakeUtcDateTime(2015, 1, 15));
            foo._dateMap.Add(3, MakeUtcDateTime(2015, 1, 16));

            foo._dateNullable = MakeUtcDateTime(2015, 1, 17);

            foo._dateRequired = MakeUtcDateTime(2015, 1, 18);
            foo._dateRequiredOptional = MakeUtcDateTime(2015, 1, 19);

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Dates));
            serializer.Serialize(foo, writer);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromDateTimeCount);
            Assert.AreEqual(0, BondTypeAliasConverter.ConvertToDateTimeCount);
        }

        [Test]
        public void CorrectRoundtripConvertCountLongStruct()
        {
            var foo = new Dates();
            foo._date = MakeUtcDateTime(2015,1,8);
            foo._dateVector.Add(MakeUtcDateTime(2015,1,9));
            foo._dateVector.Add(MakeUtcDateTime(2015,1,10));
            foo._dateList.AddLast(MakeUtcDateTime(2015, 1, 11));
            foo._dateList.AddLast(MakeUtcDateTime(2015, 1, 12));
            foo._dateList.AddLast(MakeUtcDateTime(2015, 1, 13));

            foo._dateMap.Add(1, MakeUtcDateTime(2015, 1, 14));
            foo._dateMap.Add(2, MakeUtcDateTime(2015, 1, 15));
            foo._dateMap.Add(3, MakeUtcDateTime(2015, 1, 16));

            foo._dateNullable = MakeUtcDateTime(2015, 1, 17);

            foo._dateRequired = MakeUtcDateTime(2015, 1, 18);
            foo._dateRequiredOptional = MakeUtcDateTime(2015, 1, 19);

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Dates));
            serializer.Serialize(foo, writer);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            var deserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Dates));
            var foo2 = deserializer.Deserialize<Dates>(reader);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromDateTimeCount);
            Assert.AreEqual(foo2.CountInstances(), BondTypeAliasConverter.ConvertToDateTimeCount);
            Assert.AreEqual(foo.CountInstances(), foo2.CountInstances());
            Assert.IsTrue(Bond.Comparer.Equal(foo, foo2));
        }

        [Test]
        public void CorrectSerializeConvertCountBlobStruct()
        {
            var foo = new Decimals();
            foo._dec = new decimal(19.91);
            foo._decVector.Add(new decimal(10.10));
            foo._decVector.Add(new decimal(11.11));
            foo._decList.AddLast(new decimal(12.12));
            foo._decList.AddLast(new decimal(13.13));
            foo._decList.AddLast(new decimal(14.14));

            foo._decMap.Add(1, new decimal(15.15));
            foo._decMap.Add(2, new decimal(16.16));
            foo._decMap.Add(3, new decimal(17.17));

            foo._decNullable = new decimal(20.20);

            foo._decRequired = new decimal(21.21);
            foo._decRequiredOptional = new decimal(22.22);

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Decimals));
            serializer.Serialize(foo, writer);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromDecimalCount);
            Assert.AreEqual(0, BondTypeAliasConverter.ConvertToDecimalCount);
        }

        [Test]
        public void CorrectRoundtripConvertCountBlobStruct()
        {
            var foo = new Decimals();
            foo._dec = new decimal(19.91);
            foo._decVector.Add(new decimal(10.10));
            foo._decVector.Add(new decimal(11.11));
            foo._decList.AddLast(new decimal(12.12));
            foo._decList.AddLast(new decimal(13.13));
            foo._decList.AddLast(new decimal(14.14));

            foo._decMap.Add(1, new decimal(15.15));
            foo._decMap.Add(2, new decimal(16.16));
            foo._decMap.Add(3, new decimal(17.17));

            foo._decNullable = new decimal(20.20);

            foo._decRequired = new decimal(21.21);
            foo._decRequiredOptional = new decimal(22.22);

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Decimals));
            serializer.Serialize(foo, writer);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            var deserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Decimals));
            var foo2 = deserializer.Deserialize<Decimals>(reader);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromDecimalCount);
            Assert.AreEqual(foo2.CountInstances(), BondTypeAliasConverter.ConvertToDecimalCount);
            Assert.AreEqual(foo.CountInstances(), foo2.CountInstances());
            Assert.IsTrue(Bond.Comparer.Equal(foo, foo2));
        }

        [Test]
        public void CorrectSerializeConvertCountRef()
        {
            var foo = new RefObjects();
            foo._ref = new RefObject("1111");
            foo._refVector.Add(new RefObject("2222"));
            foo._refVector.Add(new RefObject("3333"));
            foo._refList.AddLast(new RefObject("4444"));
            foo._refList.AddLast(new RefObject("5555"));
            foo._refList.AddLast(new RefObject("6666"));

            foo._refMap.Add(1, new RefObject("7777"));
            foo._refMap.Add(2, new RefObject("8888"));
            foo._refMap.Add(3, new RefObject("9999"));

            foo._refNullable = new RefObject("10010");

            foo._refRequired = new RefObject("11011");
            foo._refRequiredOptional = new RefObject("12012");

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(RefObjects));

            serializer.Serialize(foo, writer);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromRefObjectCount);
            Assert.AreEqual(0, BondTypeAliasConverter.ConvertToRefObjectCount);
        }

        [Test]
        public void CorrectRoundtripConvertCountRef()
        {
            var foo = new RefObjects();
            foo._ref = new RefObject("1111");
            foo._refVector.Add(new RefObject("2222"));
            foo._refVector.Add(new RefObject("3333"));
            foo._refList.AddLast(new RefObject("4444"));
            foo._refList.AddLast(new RefObject("5555"));
            foo._refList.AddLast(new RefObject("6666"));

            foo._refMap.Add(1, new RefObject("7777"));
            foo._refMap.Add(2, new RefObject("8888"));
            foo._refMap.Add(3, new RefObject("9999"));

            foo._refNullable = new RefObject("10010");

            foo._refRequired = new RefObject("11011");
            foo._refRequiredOptional = new RefObject("12012");

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(RefObjects));
            serializer.Serialize(foo, writer);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            var deserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(RefObjects));
            var foo2 = deserializer.Deserialize<RefObjects>(reader);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromRefObjectCount);
            Assert.AreEqual(foo2.CountInstances(), BondTypeAliasConverter.ConvertToRefObjectCount);
            Assert.AreEqual(foo.CountInstances(), foo2.CountInstances());
            Assert.IsTrue(Bond.Comparer.Equal(foo, foo2));
        }

        private static DateTime MakeUtcDateTime(int year, int month, int day)
        {
            return new DateTime(year, month, day, 0, 0, 0, DateTimeKind.Utc);
        }
    }
}
