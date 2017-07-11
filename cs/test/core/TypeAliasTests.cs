namespace UnitTest.Aliases
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Globalization;
    using System.Linq;
    using System.Reflection;
    using Bond;
    using NUnit.Framework;

    public class Lazy<T> : IBonded<T>
    {
        readonly IBonded<T> bonded;
        T instance;

        public Lazy()
        {
            bonded = Bonded<T>.Empty;
        }

        public Lazy(IBonded bonded)
        {
            this.bonded = bonded.Convert<T>();
        }

        public Lazy(T instance)
        {
            this.instance = instance;
            this.bonded = new Bonded<T>(instance);
        }

        public T Value
        {
            get
            {
                if (instance == null)
                    instance = bonded.Deserialize();
                return instance;
            }
        }

        public T Deserialize()
        {
            return bonded.Deserialize();
        }

        public void Serialize<W>(W writer)
        {
            bonded.Serialize(writer);
        }

        public U Deserialize<U>()
        {
            return bonded.Deserialize<U>();
        }

        public IBonded<U> Convert<U>()
        {
            return bonded.Convert<U>();
        }

        public static bool operator ==(Lazy<T> left, Lazy<T> right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(Lazy<T> left, Lazy<T> right)
        {
            return !(left == right);
        }

        public override bool Equals(object obj)
        {
            return Bond.Comparer.Equal(Value, (obj as Lazy<T>).Value);
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }

    public static class BondTypeAliasConverter
    {
        public static decimal Convert(ArraySegment<byte> value, decimal unused)
        {
            var bits = new int[value.Count / sizeof(int)];
            Buffer.BlockCopy(value.Array, value.Offset, bits, 0, bits.Length * sizeof(int));
            return new decimal(bits);
        }

        public static ArraySegment<byte> Convert(decimal value, ArraySegment<byte> unused)
        {
            var bits = decimal.GetBits(value);
            var data = new byte[bits.Length * sizeof(int)];
            Buffer.BlockCopy(bits, 0, data, 0, data.Length);
            return new ArraySegment<byte>(data);
        }

        public static byte[] Convert(ArraySegment<byte> value, byte[] unused)
        {
            var arr = new byte[value.Count];
            Buffer.BlockCopy(value.Array, value.Offset, arr, 0, value.Count);
            return arr;
        }

        public static ArraySegment<byte> Convert(byte[] value, ArraySegment<byte> unused)
        {
            return new ArraySegment<byte>(value);
        }

        #region Box<T> for basic types

        public static bool Convert(Box<bool> value, bool unused)
        {
            return value.Value;
        }

        public static Box<bool> Convert(bool value, Box<bool> unused)
        {
            return new Box<bool>(value);
        }

        public static SByte Convert(Box<SByte> value, SByte unused)
        {
            return value.Value;
        }

        public static Box<SByte> Convert(SByte value, Box<SByte> unused)
        {
            return new Box<SByte>(value);
        }

        public static Int16 Convert(Box<Int16> value, Int16 unused)
        {
            return value.Value;
        }

        public static Box<Int16> Convert(Int16 value, Box<Int16> unused)
        {
            return new Box<Int16>(value);
        }

        public static Int32 Convert(Box<Int32> value, Int32 unused)
        {
            return value.Value;
        }

        public static Box<Int32> Convert(Int32 value, Box<Int32> unused)
        {
            return new Box<Int32>(value);
        }

        public static Int64 Convert(Box<Int64> value, Int64 unused)
        {
            return value.Value;
        }

        public static Box<Int64> Convert(Int64 value, Box<Int64> unused)
        {
            return new Box<Int64>(value);
        }

        public static Byte Convert(Box<Byte> value, Byte unused)
        {
            return value.Value;
        }

        public static Box<Byte> Convert(Byte value, Box<Byte> unused)
        {
            return new Box<Byte>(value);
        }

        public static UInt16 Convert(Box<UInt16> value, UInt16 unused)
        {
            return value.Value;
        }

        public static Box<UInt16> Convert(UInt16 value, Box<UInt16> unused)
        {
            return new Box<UInt16>(value);
        }

        public static UInt32 Convert(Box<UInt32> value, UInt32 unused)
        {
            return value.Value;
        }

        public static Box<UInt32> Convert(UInt32 value, Box<UInt32> unused)
        {
            return new Box<UInt32>(value);
        }

        public static UInt64 Convert(Box<UInt64> value, UInt64 unused)
        {
            return value.Value;
        }

        public static Box<UInt64> Convert(UInt64 value, Box<UInt64> unused)
        {
            return new Box<UInt64>(value);
        }

        public static float Convert(Box<float> value, float unused)
        {
            return value.Value;
        }

        public static Box<float> Convert(float value, Box<float> unused)
        {
            return new Box<float>(value);
        }

        public static double Convert(Box<double> value, double unused)
        {
            return value.Value;
        }

        public static Box<double> Convert(double value, Box<double> unused)
        {
            return new Box<double>(value);
        }

        public static string Convert(Box<string> value, string unused)
        {
            return value.Value ?? string.Empty;
        }

        public static Box<string> Convert(string value, Box<string> unused)
        {
            return new Box<string>(string.IsNullOrEmpty(value) ? null : value);
        }

        #endregion
    }

    [TestFixture]
    public class TypeAliasTests
    {
        [Test]
        public void GenericTypeAlias()
        {
            var from = UnitTest.Random.Init<GenericAlias>();
            TestTypeAliases(from);

            from = new GenericAlias
            {
                bar = new Alias.EnumString<Bar>(Bar.One)
            };

            TestTypeAliases(from);
        }

        [Test]
        public void AliasContainer()
        {
            var from = UnitTest.Random.Init<ContainerAlias>();
            TestTypeAliases<ContainerAlias, ContainerNotAliased>(from);
        }

        [Test]
        public void ArrayResize()
        {
            // Generate an array so that it has to be resized twice when we
            // deserialize it with the SimpleXmlParser.
            //
            // More particularly, we generate an array that's beyond the hard
            // limit of 64 items. The SimpleXmlParser is streaming, so it will
            // tell the DeserializerTransform that the size is 0. So, when we
            // attempt to add an element to the array, it will immediately
            // resize the array to have 64 spots. Then, when we reach 64
            // elements, we resize the array to be 128. NB, the SimpleXmlParser
            // is called in the extensive TestTypeAlias suite, which will
            // attempt to transcode.

            var seed = (int)DateTime.Now.ToBinary();
            var r = new System.Random(seed);
            System.Diagnostics.Debug.WriteLine("Random seed {0}", seed);

            var target = new bool[66];
            foreach (var i in Enumerable.Range(0, 66))
            {
                target[i] = r.Next(0, 2) == 1;
            }

            var data = UnitTest.Random.Init<ContainerAlias>();
            data.arrayContainer = target;

            TestTypeAliases<ContainerAlias, ContainerNotAliased>(data);
        }

        [Test]
        public void AliasBlob()
        {
            var from = InitBlobAlias();

            TestTypeAliases<BlobAlias, BlobNotAliased>(from);
        }

        [Test]
        public void AliasBonded()
        {
            var from = new BondedAlias {lazy = new Lazy<Foo>(UnitTest.Random.Init<Foo>())};
            Assert.IsTrue(Reflection.IsBonded(typeof(Lazy<Foo>)));
            TestTypeAliases(from);
        }

        [Test]
        public void AliasGenericBonded()
        {
            var from = new GenericBondedAlias<Foo> { lazy = new Lazy<Foo>(UnitTest.Random.Init<Foo>()) };
            Assert.IsTrue(Reflection.IsBonded(typeof(Lazy<Foo>)));
            TestTypeAliases(from);
        }

        [Test]
        public void AliasesInField()
        {
            var from = new FieldOfStructWithAliases
            {
                b = InitBlobAlias()
            };

            TestTypeAliases(from);
        }

        [Test]
        public void AliasesInContainer()
        {
            var from = new ContainerOfStructWithAliases
            {
                m = {{"foo", new List<BlobAlias> {InitBlobAlias()}}}
            };
            TestTypeAliases(from);
        }

        [Test]
        public void AliasesInBase()
        {
            var from = new BaseWithAliases
            {
                x = decimal.One / 3m,
                y = { 100000000000000000m, -9999999999999999999999m },
                z = 79228162514264337593543950335m
            };
            TestTypeAliases(from);
        }

        [Test]
        public void AliasesInNested()
        {
            var from = new NestedWithAliases
            {
                f = new FieldOfStructWithAliases
                {
                    b = InitBlobAlias()
                }
            };
            TestTypeAliases(from);
        }

        // We test on the SchemaFields instead of the RuntimeSchema, because, for now, the list sub
        // type is not part of Bond.TypeDef
        [Test]
        public void AliasesListDataType()
        {
            var schemaFields = typeof(ContainerAlias).GetSchemaFields();

            foreach (var field in schemaFields)
            {
                ListSubType fieldListSubType = field.GetSchemaType().GetBondListDataType();

                switch (field.Name)
                {
                    case "customListFoo":
                        Assert.AreEqual(ListSubType.NULLABLE_SUBTYPE, fieldListSubType);
                        break;

                    case "arrayBlob":
                        Assert.AreEqual(ListSubType.BLOB_SUBTYPE, fieldListSubType);
                        break;

                    default:
                        Assert.AreEqual(ListSubType.NO_SUBTYPE, fieldListSubType, "Failed on field '{0}'", field.Name);
                        break;
                }
            }
        }

        [Test]
        public void AliasesWithConverter()
        {
            var converted = new ConvertedTypes();
            TestTypeAliases(converted);

            converted = new ConvertedTypes
            {
                c_bool = true,
                c_int8 = -8,
                c_int16 = -16,
                c_int32 = -32,
                c_int64 = -64,
                c_uint8 =  8,
                c_uint16 = 16,
                c_uint32 = 32,
                c_uint64 = 64,
                c_float = 4.0f,
                c_double = 8.0,
                c_string = "string",
                c_wstring = "wstring"
            };
            TestTypeAliases(converted);
        }

        static BlobAlias InitBlobAlias()
        {
            return new BlobAlias
            {
                x = decimal.One / 3m,
                y = { 100000000000000000m, -9999999999999999999999m },
                z = 79228162514264337593543950335m
            };
        }

        static void TestTypeAliases<T>(T from) where T : class
        {
            TestTypeAliases<T, T>(from);
        }

        static void TestTypeAliases<T, U>(T from) where T : class
        {
            Util.AllSerializeDeserialize<T, T>(from);

            var to = Clone<T>.From(Clone<U>.From(from));
            Assert.IsTrue(from.IsEqual<T>(to));
        }
    }

    public struct Box<T>
    {
        public T Value;

        public Box(T value)
        {
            Value = value;
        }

        public static implicit operator Box<T>(T value)
        {
            return new Box<T>(value);
        }

        public static bool operator ==(Box<T> left, Box<T> right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(Box<T> left, Box<T> right)
        {
            return !(left == right);
        }

        // We must provide implementations of operator == and operator != to
        // use this type in our tests. However, it is a C# compiler error to
        // implement these operators without overriding Equals and GetHashCode.
        // However, the default implementation of these methods is sufficient.
        // Hence, we have these apparently useless overrides.
        //
        // The name of the ReSharper warning is not spelled correctly.
        //
        // ReSharper disable RedundantOverridenMember
        public override bool Equals(object obj)
        {
            return base.Equals(obj);
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
        // ReSharper restore RedundantOverridenMember
    }

    [Bond.Schema]
    public class ConvertedTypes
    {
        [Bond.Id(0), Bond.Type(typeof(bool))]
        public Box<bool> c_bool;

        [Bond.Id(1), Bond.Type(typeof(sbyte))]
        public Box<sbyte> c_int8;

        [Bond.Id(2), Bond.Type(typeof(short))]
        public Box<short> c_int16;

        [Bond.Id(3), Bond.Type(typeof(int))]
        public Box<int> c_int32;

        [Bond.Id(4), Bond.Type(typeof(long))]
        public Box<long> c_int64;

        [Bond.Id(5), Bond.Type(typeof(byte))]
        public Box<byte> c_uint8;

        [Bond.Id(6), Bond.Type(typeof(ushort))]
        public Box<ushort> c_uint16;

        [Bond.Id(7), Bond.Type(typeof(uint))]
        public Box<uint> c_uint32;

        [Bond.Id(8), Bond.Type(typeof(ulong))]
        public Box<ulong> c_uint64;

        [Bond.Id(9), Bond.Type(typeof(float))]
        public Box<float> c_float;

        [Bond.Id(10), Bond.Type(typeof(double))]
        public Box<double> c_double;

        [Bond.Id(11), Bond.Type(typeof(string))]
        public Box<string> c_string;

        [Bond.Id(12), Bond.Type(typeof(Bond.Tag.wstring))]
        public Box<string> c_wstring;
    }

    // An extremely simple example of a custom container implementation.
    public class SomeCustomList<T> : ICollection<T>, ICollection
    {
        readonly List<T> backingList = new List<T>();

        public IEnumerator<T> GetEnumerator()
        {
            return backingList.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable)backingList).GetEnumerator();
        }

        public void Add(T item)
        {
            backingList.Add(item);
        }

        public void Clear()
        {
            backingList.Clear();
        }

        public bool Contains(T item)
        {
            return backingList.Contains(item);
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            backingList.CopyTo(array, arrayIndex);
        }

        public bool Remove(T item)
        {
            return backingList.Remove(item);
        }

        public void CopyTo(Array array, int index)
        {
            ((ICollection)backingList).CopyTo(array, index);
        }

        public int Count
        {
            get { return backingList.Count; }
        }

        public object SyncRoot
        {
            get { return ((ICollection)backingList).SyncRoot; }
        }

        public bool IsSynchronized
        {
            get { return ((ICollection)backingList).IsSynchronized; }
        }

        bool ICollection<T>.IsReadOnly
        {
            get { return ((ICollection<T>)backingList).IsReadOnly; }
        }
    }
}
