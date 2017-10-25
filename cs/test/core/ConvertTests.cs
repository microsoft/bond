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
        }

        internal int CountInstances()
        {
            return 1 + _decVector.Count + _decList.Count + _decMap.Count + (_decNullable.HasValue ? 1 : 0) + 1;
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
        }

        internal int CountInstances()
        {
            return 1 + _refVector.Count + _refList.Count + _refMap.Count + (_refNullable != null ? 1 : 0) + 1;
        }
    }

    public static class BondTypeAliasConverter
    {
        public static int ConvertToDecimalCount = 0;
        public static int ConvertFromDecimalCount = 0;

        public static int ConvertToRefObjectCount = 0;
        public static int ConvertFromRefObjectCount = 0;

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
        public void CorrectSerializeConvertCountStruct()
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

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Decimals));
            serializer.Serialize(foo, writer);

            Assert.AreEqual(foo.CountInstances(), BondTypeAliasConverter.ConvertFromDecimalCount);
            Assert.AreEqual(0, BondTypeAliasConverter.ConvertToDecimalCount);
        }

        [Test]
        public void CorrectRoundtripConvertCountStruct()
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

    }
}
