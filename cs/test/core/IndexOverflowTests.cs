using Bond;
using Bond.IO.Safe;
using Bond.Protocols;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using UnitTest.Aliases;

namespace UnitTest
{
    [TestFixture]
    class IndexOverflowTests
    {
        #region CompactBinaryReader
        [Test]
        public void CompactBinaryReader_ReadContainer_NegativeCount_Throws()
        {
            var simpleTypesWithNegativeCount = new byte[]
            {
                0xc9,
                0x0b, // list
                0x03, // subtype string
                0x41,
                0x42,
                0x43,
                0xca,
                0x0d,
                0xcc,
                0x8, // double
                0x8, // subtype set double

                0xff, // count -1
                0xff,
                0xff,
                0xff,
                0xff,
            };

            var input = new InputBuffer(simpleTypesWithNegativeCount);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<SimpleContainers>.From(reader));

            var mapWithNegativeCount = new byte[]
            {
                0x0d, // map
                0x09, // key: string
                0x02, // value: bool

                0xff, // count (5 byte)
                0xff,
                0xff,
                0xff,
                0xff,

                0x03,// str len
                0x61,
                0x62,
                0x01,
                0x00,
            };

            input = new InputBuffer(mapWithNegativeCount);
            reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Maps>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_ReadString_NegativeLength_Throws()
        {
            var stringWithNagativeLength = new byte[]
            {
                0x49, // string

                0xff, // count (5 byte)
                0xff,
                0xff,
                0xff,
                0xff,

                0x61,
            };

            var input = new InputBuffer(stringWithNagativeLength);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_ReadWString_OverflowLength_Throws()
        {
            var wstringWithOverflowLength = new byte[]
            {
                0x72, // wstring

                0xff, // count (5 byte)
                0xff,
                0xff,
                0xff,
                0xff,

                0x61,
                0x00,
            };

            var input = new InputBuffer(wstringWithOverflowLength);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));

            wstringWithOverflowLength = new byte[]
            {
                0x72, // wstring

                0x80, // count (5 byte) = int.MaxValue / 2 + 1
                0x80,
                0x80,
                0x80,
                0x04,

                0x61,
                0x00,
            };

            input = new InputBuffer(wstringWithOverflowLength);
            reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_Skip_String_NegativeLength_Throws()
        {
            var data = new byte[]
            {
                0x0a,
                0x49,

                0xff, // count -1
                0xff,
                0xff,
                0xff,
                0xff,

                0x3c,
            };

            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_Skip_WString_OverflowLength_Throws()
        {
            var data = new byte[]
            {
                0x0a,
                0x49,
                0x06,
                0x3c,
                0x33,
                0x48,
                0x25,
                0x5b,
                0x67,
                0x72,

                0xff, // count -1
                0xff,
                0xff,
                0xff,
                0xff,

                0x4d,
                0x00,
            };

            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));

            data = new byte[]
            {
                0x0a,
                0x49,
                0x06,
                0x3c,
                0x33,
                0x48,
                0x25,
                0x5b,
                0x67,
                0x72,

                0x80, // count (5 byte) = int.MaxValue / 2 + 1
                0x80,
                0x80,
                0x80,
                0x04,

                0x4d,
                0x00,
            };

            input = new InputBuffer(data);
            reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_SkipStruct_NegativeLength_Throws()
        {
            var data = new byte[]
            {
                0x26,
                0x0a,

                0xff, // count -1
                0xff,
                0xff,
                0xff,
                0xff,

                0x02,
            };

            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input, 2);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_SkipContainer_Float_OverflowLength_Throws()
        {
            var data = new byte[]
            {
                0x2b,
                0x07,

                0x80, // int.MaxValue / sizeof(float) + 1
                0x80,
                0x80,
                0x80,
                0x02,

                0x48,
                0x00,
            };

            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondClass<List<double>>>.From(reader));
        }

        [Test]
        public void CompactBinaryReader_SkipContainer_Double_OverflowLength_Throws()
        {
            var data = new byte[]
            {
                0x2b,
                0x08,

                0x80, // int.MaxValue / sizeof(double) + 1
                0x80,
                0x80,
                0x80,
                0x01,

                0x48,
            };

            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondClass<List<double>>>.From(reader));
        }
        #endregion

        #region FastBinaryReader
        [Test]
        public void FastBinaryReader_ReadContainer_NegativeCount_Throws()
        {
            var simpleTypesWithNegativeCount = new byte[]
            {
                0x0b, // list of strings
                0x00,
                0x00,
                0x09,

                0xff, // count (5 bytes)
                0xff,
                0xff,
                0xff,
                0xff,

                0x01, // sting len
                0x61,
            };

            var input = new InputBuffer(simpleTypesWithNegativeCount);
            var reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<SimpleContainers>.From(reader));

            var mapWithNegativeCount = new byte[]
            {
                0x0d, // map (string --> bool)
                0x00,
                0x00,
                0x09,
                0x02,

                0xff, // count (5 bytes)
                0xff,
                0xff,
                0xff,
                0xff,

                0x01, // string len 3
                0x61,
                0x01, // bool
            };

            input = new InputBuffer(mapWithNegativeCount);
            reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Maps>.From(reader));
        }

        [Test]
        public void FastBinaryReader_ReadString_NegativeLength_Throws()
        {
            var stringWithNagativeLength = new byte[]
            {
                0x09, // string
                0x02,
                0x00,

                0xff, // count (5 byte)
                0xff,
                0xff,
                0xff,
                0xff,

                0x61,
            };

            var input = new InputBuffer(stringWithNagativeLength);
            var reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));
        }

        [Test]
        public void FastBinaryReader_ReadWString_OverflowLength_Throws()
        {
            var data = new byte[]
            {
                0x12, // wstring
                0x03,
                0x00,

                0xff, // count (5 byte)
                0xff,
                0xff,
                0xff,
                0xff,

                0x61,
                0x00,
            };

            var input = new InputBuffer(data);
            var reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));

            data = new byte[]
            {
                0x12, // wstring
                0x03,
                0x00,

                0x80, // count (5 byte) = int.MaxValue / 2 + 1
                0x80,
                0x80,
                0x80,
                0x04,

                0x61,
                0x00,
            };

            input = new InputBuffer(data);
            reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));
        }

        [Test]
        public void FastBinaryReader_Skip_String_NegativeLength_Throws()
        {
            var data = new byte[]
            {
                0x0a,
                0x00,
                0x00,
                0x09,
                0x02,
                0x00,

                0xff, // count -1
                0xff,
                0xff,
                0xff,
                0xff,

                0x5a,
            };

            var input = new InputBuffer(data);
            var reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));
        }

        [Test]
        public void FastBinaryReader_Skip_WString_OverflowLength_Throws()
        {
            var data = new byte[]
            {
                0x0a,
                0x00,
                0x00,
                0x09,
                0x02,
                0x00,
                0x27,
                0x49,
                0x39,
                0x54,
                0x36,
                0x75,
                0x29,
                0x53,
                0x43,
                0x56,
                0x3d,
                0x21,
                0x74,
                0x56,
                0x46,
                0x39,
                0x22,
                0x7a,
                0x58,
                0x34,
                0x70,
                0x2d,
                0x70,
                0x69,
                0x5b,
                0x27,
                0x23,
                0x40,
                0x32,
                0x60,
                0x2c,
                0x43,
                0x3a,
                0x3f,
                0x6c,
                0x36,
                0x55,
                0x38,
                0x7a,
                0x54,
                0x12,
                0x03,
                0x00,

                0xff, // count -1
                0xff,
                0xff,
                0xff,
                0xff,

                0x3c,
                0x00,

            };

            var input = new InputBuffer(data);
            var reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));

            data = new byte[]
            {
                0x0a,
                0x00,
                0x00,
                0x09,
                0x02,
                0x00,
                0x27,
                0x49,
                0x39,
                0x54,
                0x36,
                0x75,
                0x29,
                0x53,
                0x43,
                0x56,
                0x3d,
                0x21,
                0x74,
                0x56,
                0x46,
                0x39,
                0x22,
                0x7a,
                0x58,
                0x34,
                0x70,
                0x2d,
                0x70,
                0x69,
                0x5b,
                0x27,
                0x23,
                0x40,
                0x32,
                0x60,
                0x2c,
                0x43,
                0x3a,
                0x3f,
                0x6c,
                0x36,
                0x55,
                0x38,
                0x7a,
                0x54,
                0x12,
                0x03,
                0x00,

                0x80, // count (5 byte) = int.MaxValue / 2 + 1
                0x80,
                0x80,
                0x80,
                0x04,

                0x3c,
                0x00,
            };

            input = new InputBuffer(data);
            reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondedAlias>.From(reader));
        }

        [Test]
        [TestCaseSource(nameof(FastBinaryOverflowingLengthPayloads))]
        public void FasttBinaryReader_SkipContainer_OverflowLength_Throws(byte[] data)
        {
            var input = new InputBuffer(data);
            var reader = new FastBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<BondClass<List<double>>>.From(reader));
        }

        public static IEnumerable<byte[]> FastBinaryOverflowingLengthPayloads()
        {
            const int TypeIndex = 3;
            byte[][] testCases =
            {
                new byte[]{(int)BondDataType.BT_UINT16, 0x80, 0x80, 0x80, 0x80, 0x04}, // Bytes: int.MaxValue / sizeof(Uint16) + 1
                new byte[]{(int)BondDataType.BT_UINT32, 0x80, 0x80, 0x80, 0x80, 0x02}, // Bytes: int.MaxValue / sizeof(Uint32) + 1
                new byte[]{(int)BondDataType.BT_UINT64, 0x80, 0x80, 0x80, 0x80, 0x01}, // Bytes: int.MaxValue / sizeof(Uint64) + 1
                new byte[]{(int)BondDataType.BT_FLOAT, 0x80, 0x80, 0x80, 0x80, 0x02}, // Bytes: int.MaxValue / sizeof(float) + 1
                new byte[]{(int)BondDataType.BT_DOUBLE, 0x80, 0x80, 0x80, 0x80, 0x01}, // Bytes: int.MaxValue / sizeof(double) + 1
                new byte[]{(int)BondDataType.BT_INT16, 0x80, 0x80, 0x80, 0x80, 0x04}, // Bytes: int.MaxValue / sizeof(Int16) + 1
                new byte[]{(int)BondDataType.BT_INT32, 0x80, 0x80, 0x80, 0x80, 0x02}, // Bytes: int.MaxValue / sizeof(Int32) + 1
                new byte[]{(int)BondDataType.BT_INT64, 0x80, 0x80, 0x80, 0x80, 0x01}, // Bytes: int.MaxValue / sizeof(Int64) + 1
            };


            foreach (var tc in testCases)
            {
                int idx = TypeIndex;
                byte[] data =
                {
                    0x0b,
                    0x01,
                    0x00,
                    0xff, // type

                    0x00, // count (5 byte)
                    0x00,
                    0x00,
                    0x00,
                    0x00,

                    0x08,
                };

                Buffer.BlockCopy(
                    src: tc,
                    srcOffset: 0,
                    dst: data,
                    dstOffset: idx,
                    count: tc.Length);

                yield return data;
            }
        }
        #endregion

        #region SimpleBinaryReader
        [Test]
        public void SimpleBinaryReader_ReadLength_NegativeLength_Throws()
        {
            var f = new Foo { _str = "abc" };
            var output = new Bond.IO.Safe.OutputBuffer(16);
            var writer = new SimpleBinaryWriter<OutputBuffer>(output, 2);
            Serialize.To(writer, f);


            var data = new byte[]
            {
                0x00,

                0xff, // count (5 byte)
                0xff,
                0xff,
                0xff,
                0xff,

                0x00,
                0x00,
                0x00,
                0x61,
            };

            var input = new InputBuffer(data);
            var reader = new SimpleBinaryReader<InputBuffer>(input);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));

            input = new InputBuffer(data);
            reader = new SimpleBinaryReader<InputBuffer>(input, 2);
            Assert.Throws<OverflowException>(() => Deserialize<Foo>.From(reader));
        }
        #endregion
    }
}
