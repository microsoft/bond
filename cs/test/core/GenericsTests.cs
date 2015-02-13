namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using NUnit.Framework;
    using System.IO;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    [TestFixture]
    public class GenericsTests
    {
        [Test]
        public void Generics()
        {
            Util.AllSerializeDeserialize<Generics, Generics>(new Generics());
            Util.AllSerializeDeserialize<Generics, Generics>(Random.Init<Generics>());
        }

        [Test]
        public void GenericsNothing()
        {
            Util.AllSerializeDeserialize<GenericsWithNothing, GenericsWithNothing>(new GenericsWithNothing());
            Util.AllSerializeDeserialize<GenericsWithNothing, GenericsWithNothing>(Random.Init<GenericsWithNothing>());
        }

        [Test]
        public void GenericInheritance()
        {
            Util.AllSerializeDeserialize<GenericInheritance, GenericInheritance>(new GenericInheritance());
            Util.AllSerializeDeserialize<GenericInheritance, GenericInheritance>(Random.Init<GenericInheritance>());
        }

        [Test]
        public void GenericWString()
        {
            Util.AllSerializeDeserialize<GenericWString, NonGenericWString>(new GenericWString());
            Util.AllSerializeDeserialize<GenericWString, NonGenericWString>(Random.Init<GenericWString>());
        }

        [Test]
        public void GenericsBonded()
        {
            var obj = new GenericBonded<Nested>();

            var field = Random.Init<Derived>();
            obj.field = new Bonded<Derived>(field);

            var poly0 = Random.Init<EmptyBase>();
            var poly1 = Random.Init<Nested>();
            var poly2 = Random.Init<Derived>();

            obj.poly.Add(new Bonded<EmptyBase>(poly0));
            obj.poly.Add(new Bonded<Nested>(poly1));
            obj.poly.Add(new Bonded<Derived>(poly2));

            var stream = new MemoryStream();
            
            Util.SerializeCB(obj, stream);
            stream.Position = 0;
            obj = Util.DeserializeCB<GenericBonded<Nested>>(stream);

            Assert.IsTrue(Comparer.Equal(field, obj.field.Deserialize<Derived>()));
            Assert.IsTrue(Comparer.Equal(poly0, obj.poly[0].Deserialize<EmptyBase>()));
            Assert.IsTrue(Comparer.Equal(poly1, obj.poly[1].Deserialize()));
            Assert.IsTrue(Comparer.Equal(poly2, obj.poly[2].Deserialize<Derived>()));

            stream.SetLength(0);
            Util.SerializeCB(obj, stream);
            stream.Position = 0;
            obj = Util.DeserializeCB<GenericBonded<Nested>>(stream);

            Assert.IsTrue(Comparer.Equal(field, obj.field.Deserialize<Derived>()));
            Assert.IsTrue(Comparer.Equal(poly0, obj.poly[0].Deserialize<EmptyBase>()));
            Assert.IsTrue(Comparer.Equal(poly1, obj.poly[1].Deserialize()));
            Assert.IsTrue(Comparer.Equal(poly2, obj.poly[2].Deserialize<Derived>()));
        }

        [Test]
        public void BondedGeneric()
        {
            var obj = new BondedGeneric();

            var field1 = Random.Init<Derived>();
            obj.cbt.nullableField = new Bonded<Derived>(field1);

            var field2 = Random.Init<GenericDerived<string>>();
            obj.cbgws.field = new Bonded<GenericClass<string>>(field2);

            var stream = new MemoryStream();
            
            Util.SerializeCB(obj, stream);
            stream.Position = 0;
            obj = Util.DeserializeCB<BondedGeneric>(stream);

            Assert.IsTrue(Comparer.Equal(field1, obj.cbt.nullableField.Deserialize<Derived>()));
            Assert.IsTrue(Comparer.Equal(field2, obj.cbgws.field.Deserialize<GenericDerived<string>>()));
        }

        [Test]
        public void GenericConstraintViolation()
        {
            try
            {
                new Deserializer<CompactBinaryReader<InputStream>>(typeof(GenericNothingClass<int>));
            }
            catch (InvalidOperationException e)
            {
                Assert.IsTrue(e.Message.StartsWith("Invalid default value "));
            }
        }

        void GenericHelperScalar<T>() where T : struct
        {
            var value = GenericFactory.Create<T>();
            var nullable = GenericFactory.Create<T?>();
            Assert.AreEqual(value, default(T));
            Assert.AreEqual(nullable, default(T?));
            GenericCompareTrue(value, default(T));
            GenericCompareTrue(nullable, default(T?));
        }

        //NUnit workaround. It can't compare two null arrays
        void GenericHelperArraySegment()
        {
            var value = GenericFactory.Create<ArraySegment<byte>>();
            var nullable = GenericFactory.Create<ArraySegment<byte>?>();

            Assert.IsNull(value.Array);
            Assert.AreEqual(0, value.Count);
            Assert.AreEqual(0, value.Offset);
            Assert.IsNull(nullable);
        }

        void GenericHelperClass<T>() where T : class, new()
        {
            var value = GenericFactory.Create<T>();
            Assert.IsTrue(Comparer.Equal(value, new T()));
        }

        void GenericCompareFalse<T>(T left, T right)
        {
            Assert.IsFalse(Comparer.Equal(left, right));
            Assert.IsFalse(Comparer.Equal(right, left));
        }

        void GenericCompareTrue<T>(T left, T right)
        {
            Assert.IsTrue(Comparer.Equal(left, right));
            Assert.IsTrue(Comparer.Equal(right, left));
        }

        [Test]
        public void GenericHelpers()
        {
            GenericHelperScalar<bool>();
            GenericHelperScalar<sbyte>();
            GenericHelperScalar<short>();
            GenericHelperScalar<int>();
            GenericHelperScalar<long>();
            GenericHelperScalar<byte>();
            GenericHelperScalar<ushort>();
            GenericHelperScalar<uint>();
            GenericHelperScalar<ulong>();
            GenericHelperScalar<float>();
            GenericHelperScalar<double>();
            GenericHelperArraySegment();

            GenericHelperClass<BasicTypes>();
            GenericHelperClass<List<BasicTypes>>();
            GenericHelperClass<List<string>>();

            GenericCompareFalse(true, false);
            GenericCompareFalse(false, true);
            GenericCompareFalse(1, 2);
            GenericCompareFalse<short>(1, 2);
            GenericCompareFalse<float>(1, 2);
            GenericCompareFalse<float?>(1, 2);
            GenericCompareFalse("abc", "abcd");
            GenericCompareFalse("abc", null);
            GenericCompareFalse(new BasicTypes(), null);
            GenericCompareFalse<double?>(10, null);
            
            GenericCompareTrue("abc", "abc");
            GenericCompareTrue<string>(null, null);
            GenericCompareTrue<int?>(null, null);
            GenericCompareTrue<int?>(10, 10);
            GenericCompareTrue<BasicTypes>(null, null);

            var blob1 = new ArraySegment<byte>(new byte[2]);
            var blob2 = new ArraySegment<byte>(blob1.Array, 1, blob1.Count - 1);

            GenericCompareFalse(blob1, blob2);
            GenericCompareFalse(blob2, blob1);

            var vectors1 = Random.Init<Vectors>();
            var vectors2 = Clone<Vectors>.From(vectors1);

            GenericCompareTrue(vectors1, vectors2);
            GenericCompareTrue(vectors1._bool, vectors2._bool);

            vectors1._bool.Add(false);
            GenericCompareFalse(vectors1, vectors2);
            GenericCompareFalse(vectors1._bool, null);
            GenericCompareFalse(vectors1._bool, vectors2._bool);
            
            vectors1._bool.Add(true);
            GenericCompareFalse(vectors1, vectors2);
            GenericCompareFalse(vectors1._bool, vectors2._bool);

            var nested1 = Random.Init<NestedContainers>();
            var nested2 = Clone<NestedContainers>.From(nested1);

            GenericCompareTrue(nested1, nested2);
            GenericCompareTrue(nested1.vvbt, nested2.vvbt);

            if (nested1.vvbt.Count != 0)
            {
                nested1.vvbt[0].Add(new BasicTypes());
                GenericCompareFalse(nested1, nested2);
                GenericCompareFalse(nested1.vvbt, nested2.vvbt);

                nested2.vvbt[0].Add(new BasicTypes());
                GenericCompareTrue(nested1, nested2);
                GenericCompareTrue(nested1.vvbt, nested2.vvbt);

                nested1.vvbt[0][0]._bool = !nested2.vvbt[0][0]._bool;
                GenericCompareFalse(nested1, nested2);
                GenericCompareFalse(nested1.vvbt, nested2.vvbt);

                nested1.vvbt[0].Add(null);
                nested2.vvbt[0].Add(new BasicTypes());
                GenericCompareFalse(nested1, nested2);
                GenericCompareFalse(nested1.vvbt, nested2.vvbt);
            }

            var derived1 = Random.Init<Derived>();
            var derived2 = Clone<Derived>.From(derived1);

            GenericCompareTrue(derived1, derived2);

            derived1.basic._bool = !derived2.basic._bool;
            GenericCompareFalse(derived1, derived2);

            derived1.basic._bool = derived2.basic._bool;
            GenericCompareTrue(derived1, derived2);
            derived1.basic._enum1 = EnumType1.EnumValue1;
            derived2.basic._enum1 = EnumType1.EnumValue2;
            GenericCompareFalse(derived1, derived2);
        }
    }
}
