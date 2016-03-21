namespace UnitTest
{
    using System.Collections.Generic;
    using System.IO;
    using Bond;
    using NUnit.Framework;


    [TestFixture]
    public class StructTests
    {
        [Test]
        public void StructWithFields()
        {
            TestStruct<StructWithFields>();
        }

        [Test]
        public void StructWithProperties()
        {
            TestStruct<StructWithProperties>();
        }

        [Test]
        public void NestedStructs()
        {
            TestStruct<NestedStructs>();
        }

        [Test]
        public void ClassWithStructFields()
        {
            TestClass<ClassWithStructFields>();
        }

        [Test]
        public void ClassWithStructProperties()
        {
            TestClass<ClassWithStructProperties>();
        }

        [Test]
        public void CollectionsOfStructs()
        {
            TestClass<CollectionsOfStructs>();
        }

        void TestClass<T>() where T : class
        {
            Util.AllSerializeDeserialize<T, T>(Random.Init<T>());
            TestCloning<T>();
        }

        void TestStruct<T>() where T : struct
        {
            TestSerialization<T>();
            TestCloning<T>();
        }

        void TestSerialization<T>()
        {
            {
                var stream = new MemoryStream();
                var from = Random.Init<T>();
                Util.SerializeCB(from, stream);
                stream.Position = 0;
                var to = Util.DeserializeCB<T>(stream);
                Assert.IsTrue(Comparer.Equal(from, to));
            }

            {
                var stream = new MemoryStream();
                var from = Random.Init<T>();
                Util.SerializeCB2(from, stream);
                stream.Position = 0;
                var to = Util.DeserializeCB2<T>(stream);
                Assert.IsTrue(Comparer.Equal(from, to));
            }
        }

        void TestCloning<T>()
        {
            var source = Random.Init<T>();
            var target = Clone<T>.From(source);
            Assert.IsTrue(Comparer.Equal(source, target));
        }
    }

    [global::Bond.Schema]
    public struct StructWithProperties
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
        public EnumType1 _enum1 { get; set; }

        [global::Bond.Id(22), global::Bond.Type(typeof(long))]
        public System.DateTime dt { get; set; }
    }

    [global::Bond.Schema]
    public struct StructWithFields
    {
        [global::Bond.Id(0)]
        public bool _bool;

        [global::Bond.Id(2)]
        public string _str;

        [global::Bond.Id(3), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string _wstr;

        [global::Bond.Id(10)]
        public ulong _uint64;

        [global::Bond.Id(11)]
        public ushort _uint16;

        [global::Bond.Id(12)]
        public uint _uint32;

        [global::Bond.Id(13)]
        public byte _uint8;

        [global::Bond.Id(14)]
        public sbyte _int8;

        [global::Bond.Id(15)]
        public short _int16;

        [global::Bond.Id(16)]
        public int _int32;

        [global::Bond.Id(17)]
        public long _int64;

        [global::Bond.Id(18)]
        public double _double;

        [global::Bond.Id(20)]
        public float _float;

        [global::Bond.Id(22), global::Bond.Type(typeof(long))]
        public System.DateTime dt;
    }

    [global::Bond.Schema]
    public struct NestedStructs
    {
        [global::Bond.Id(0)]
        public StructWithFields s1;

        [global::Bond.Id(1)]
        public StructWithProperties s2 { get; set; }
    }

    [global::Bond.Schema]
    public class ClassWithStructFields
    {
        [global::Bond.Id(0)]
        public StructWithFields s1;

        [global::Bond.Id(1)]
        public StructWithProperties s2;
    }

    [global::Bond.Schema]
    public class ClassWithStructProperties
    {
        [global::Bond.Id(0)]
        public StructWithFields s1 { get; set; }

        [global::Bond.Id(1)]
        public StructWithProperties s2 { get; set; }
    }

    [global::Bond.Schema]
    public class CollectionsOfStructs
    {
        [global::Bond.Id(0)]
        public List<StructWithFields> l1 { get; set; }

        [global::Bond.Id(1)]
        public List<StructWithProperties> l2 { get; set; }

        [global::Bond.Id(2)]
        public Dictionary<int, StructWithProperties> m1 { get; set; }

        [global::Bond.Id(3)]
        public NestedStructs[] a1 { get; set; }
    }
}
