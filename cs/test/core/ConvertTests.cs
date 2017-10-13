namespace UnitTest.Convert
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Globalization;
    using System.Linq;
    using System.Reflection;
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
        }

        internal int CountDecimals()
        {
            return 1 + _decVector.Count + _decList.Count + _decMap.Count + (_decNullable.HasValue ? 1 : 0);
        }
    }

    public static class BondTypeAliasConverter
    {
        public static int ConvertToDecimalCount = 0;
        public static int ConvertToArraySegmentCount = 0;

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

            Interlocked.Increment(ref ConvertToArraySegmentCount);

            return new ArraySegment<byte>(data);
        }

        public static void ResetCounts()
        {
            ConvertToDecimalCount = 0;
            ConvertToArraySegmentCount = 0;
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
        public void CorrectSerializeConvertCount()
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

            var outputStream = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(outputStream);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Decimals));

            serializer.Serialize(foo, writer);

            Assert.AreEqual(foo.CountDecimals(), BondTypeAliasConverter.ConvertToArraySegmentCount);
            Assert.AreEqual(0, BondTypeAliasConverter.ConvertToDecimalCount);
        }

        [Test]
        public void CorrectRoundtripConvertCount()
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

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Decimals));

            serializer.Serialize(foo, writer);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var deserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Decimals));
            var foo2 = deserializer.Deserialize<Decimals>(reader);

            Assert.AreEqual(foo.CountDecimals(), BondTypeAliasConverter.ConvertToArraySegmentCount);
            Assert.AreEqual(foo2.CountDecimals(), BondTypeAliasConverter.ConvertToDecimalCount);
            Assert.AreEqual(foo.CountDecimals(), foo2.CountDecimals());
            Assert.IsTrue(Bond.Comparer.Equal(foo, foo2));
        }
    }
}
