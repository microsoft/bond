namespace UnitTest
{
    using System;
    using System.IO;
    using NUnit.Framework;
    using Bond;
    using Bond.Protocols;
    using Bond.IO;
    using Bond.IO.Unsafe;
    using Bond.Internal.Reflection;

    [TestFixture]
    public class BondedTests
    {
        // Deserialize T from IBonded<T> containing an instance or payload of derived class.
        void BondedDeserialize<T, D>() 
            where T : class
            where D : class, T, new()
        {
            var from = Random.Init<D>();

            IBonded<T> bondedInstance = new Bonded<D>(from);
            IBonded<T> bondedPayloadCB = Util.MakeBondedCB(from);
            IBonded<T> bondedPayloadCB2 = Util.MakeBondedCB2(from);
            IBonded<T> bondedPayloadSP = Util.MakeBondedSP(from);
            IBonded<BondClass<IBonded<T>>> nestedBonded =
                new Bonded<BondClass<IBonded<T>>>(new BondClass<IBonded<T>> { field = bondedInstance });

            for (var i = 2; --i != 0;)
            {
                var to1 = bondedInstance.Deserialize();
                var to2 = bondedPayloadCB.Deserialize();
                var toCb2 = bondedPayloadCB2.Deserialize();
                var to3 = bondedPayloadSP.Deserialize();

                Assert.IsTrue(to1.IsEqual<T>(from));
                Assert.IsTrue(to2.IsEqual<T>(from));
                Assert.IsTrue(toCb2.IsEqual<T>(from));
                Assert.IsTrue(to3.IsEqual<T>(from));
                Assert.IsTrue(nestedBonded.Deserialize().field.Deserialize().IsEqual(from));
            }
        }

        // Deserialize derived class from IBonded<T> containing an instance or payload of derived class.
        void BondedDowncastDeserialize<T, D>()
            where T : class
            where D : class, T, new()
        {
            var from = Random.Init<D>();

            IBonded<T> bondedInstance = new Bonded<D>(from);
            IBonded<T> bondedPayloadCB = Util.MakeBondedCB(from);
            IBonded<T> bondedPayloadCB2 = Util.MakeBondedCB2(from);
            IBonded<T> bondedPayloadSP = Util.MakeBondedSP(from);

            for (var i = 2; --i != 0;)
            {
                var to1 = bondedInstance.Deserialize<D>();
                var to2 = bondedPayloadCB.Deserialize<D>();
                var toCb2 = bondedPayloadCB2.Deserialize<D>();
                var to3 = bondedPayloadSP.Deserialize<D>();

                Assert.IsTrue(to1.IsEqual<D>(from));
                Assert.IsTrue(to2.IsEqual<D>(from));
                Assert.IsTrue(toCb2.IsEqual<D>(from));
                Assert.IsTrue(to3.IsEqual<D>(from));
            }
        }

        // Serialize IBonded<T> (transcoding to different protocol) 
        void BondedSerialize<T, D>(D from, IBonded<T> bonded)
            where T : class
            where D : class, T, new()
        {
            Util.RoundtripStream<IBonded<T>, D> streamRoundtrip = (serialize, deserialize) =>
            {
                var stream = new MemoryStream();
                serialize(bonded, stream);
                stream.Position = 0;
                var to = deserialize(stream);

                Assert.IsTrue(to.IsEqual<D>(from));
            };

            streamRoundtrip(Util.SerializeCB, Util.DeserializeCB<D>);
            streamRoundtrip(Util.SerializeCB2, Util.DeserializeCB2<D>);
            streamRoundtrip(Util.SerializeFB, Util.DeserializeFB<D>);
            streamRoundtrip(Util.SerializeSP, Util.DeserializeSP<D, D>);
            streamRoundtrip(Util.SerializeXml, Util.DeserializeXml<D>);
        }

        void BondedSerialize<D>(D from, IBonded bonded)
            where D : class, new()
        {
            Util.RoundtripStream<IBonded, D> streamRoundtrip = (serialize, deserialize) =>
            {
                var stream = new MemoryStream();
                serialize(bonded, stream);
                stream.Position = 0;
                var to = deserialize(stream);

                Assert.IsTrue(to.IsEqual<D>(from));
            };

            streamRoundtrip(Util.SerializeCB, Util.DeserializeCB<D>);
            streamRoundtrip(Util.SerializeCB2, Util.DeserializeCB2<D>);
            streamRoundtrip(Util.SerializeFB, Util.DeserializeFB<D>);
        }

        void BondedSerialize<T, D>()
            where T : class
            where D : class, T, new()
        {
            var from = Random.Init<D>();

            IBonded<T> bondedInstance = new Bonded<D>(from);
            IBonded<T> bondedPayloadCB = Util.MakeBondedCB(from);
            IBonded<T> bondedPayloadCB2 = Util.MakeBondedCB2(from);
            IBonded<T> bondedPayloadSP = Util.MakeBondedSP(from);

            BondedSerialize(from, bondedInstance);
            BondedSerialize(from, bondedPayloadCB);
            BondedSerialize(from, bondedPayloadCB2);
            BondedSerialize(from, bondedPayloadSP);

            BondedSerialize(from, (IBonded)bondedPayloadCB);
            BondedSerialize(from, (IBonded)bondedPayloadCB2);
        }

        // Serialize a class with IBonded<From> field an deserialize into class with non-lazy struct To field.
        void NonLazyDeserialization<From, To>(
            Action<BondClass<IBonded<From>>, Stream> serialize,
            Func<Stream, BondClass<To>> deserialize)
            where From : class, new()
            where To : class
        {
            var field = Random.Init<From>();
            var from = new BondClass<IBonded<From>>();
            from.field = new Bonded<From>(field);

            var stream = new MemoryStream();
            serialize(from, stream);
            stream.Position = 0;
            var to = deserialize(stream);

            Assert.IsTrue(field.IsEqual(to.field));
        }

        void NonLazyDeserializationAll<From, To>()
            where From : class, new()
            where To : class
        {
            NonLazyDeserialization<From, To>(
                Util.SerializeCB, Util.DeserializeCB<BondClass<To>>);

            NonLazyDeserialization<From, To>(
                Util.SerializeCB2, Util.DeserializeCB2<BondClass<To>>);

            NonLazyDeserialization<From, To>(
                Util.SerializeSP, Util.DeserializeSP<BondClass<IBonded<From>>, BondClass<To>>);

            NonLazyDeserialization<From, To>(
                Util.SerializeXmlWithNamespaces, Util.DeserializeXml<BondClass<To>>);
        }


        // Serialize class with struct From field and deserialize as class with IBonded<To> field.
        void LazyDeserialization<From, To, R>(
            Action<BondClass<From>, Stream> serialize,
            Func<Stream, BondClass<IBonded<To>>> deserialize)
            where From : class, new()
            where To : class 
            where R : ICloneable<R>
        {
            var from = Random.Init<BondClass<From>>();
            var stream = new MemoryStream();
            
            serialize(from, stream);
            stream.Position = 0;
            var to = deserialize(stream);

            Assert.IsTrue(from.field.IsEqual(to.field.Deserialize()));

            var deserializer = new Deserializer<R>(typeof (To), Schema<From>.RuntimeSchema);
            Assert.IsTrue(from.field.IsEqual(deserializer.Deserialize(to.field)));

            Assert.IsTrue(from.field.IsEqual<From>(to.field.Deserialize<From>()));
            Assert.IsTrue(from.field.IsEqual<From>(to.field.Convert<From>().Deserialize()));
        }

        void LazyDeserializationAll<From, To>()
            where From : class, new()
            where To : class
        {
            LazyDeserialization<From, To, CompactBinaryReader<InputStream>>(
                Util.SerializeCB, Util.DeserializeCB<BondClass<IBonded<To>>>);

            LazyDeserialization<From, To, CompactBinaryReader<InputStream>>(
                Util.SerializeCB2, Util.DeserializeCB2<BondClass<IBonded<To>>>);

            LazyDeserialization<From, To, SimpleBinaryReader<InputStream>>(
                Util.SerializeSP, Util.DeserializeSP<BondClass<From>, BondClass<IBonded<To>>>);
        }

        // Serialize class with a IBonded<Through> field containing value From.
        // Deserialize and then deserialize object Through and To from the bonded<Through> field.
        void PolymorphicDeserialization<From, Through, To, R>(
            Action<BondClass<IBonded<Through>>, Stream> serialize,
            Func<Stream, BondClass<IBonded<Through>>> deserialize)
            where From : class, Through, new() 
            where Through : class, new() 
            where To : class, new() 
            where R : ICloneable<R>
        {
            var field = Random.Init<From>();
            var from = new BondClass<IBonded<Through>>();
            from.field = new Bonded<From>(field);

            var stream = new MemoryStream();
            
            serialize(from, stream);
            stream.Position = 0;
            var to = deserialize(stream);

            Assert.IsTrue(field.IsEqual(to.field.Deserialize()));
            Assert.IsTrue(field.IsEqual(to.field.Deserialize<To>()));
            Assert.IsTrue(field.IsEqual(to.field.Convert<To>().Deserialize()));

            // bonded<T> for untagged protocol in not serialized using the protocol R
            // but instead is marshaled using a tagged protocol.
            if (!typeof(IUntaggedProtocolReader).IsAssignableFrom(typeof(R)))
            {
                var deserializer = new Deserializer<R>(typeof(To), Schema<From>.RuntimeSchema);
                Assert.IsTrue(field.IsEqual(deserializer.Deserialize(to.field.Convert<To>())));
            }
        }

        void PolymorphicDeserializationAll<From, Through, To>()
            where From : class, Through, new() 
            where Through : class, new()
            where To : class, new()
        {
            PolymorphicDeserialization<From, Through, To, CompactBinaryReader<InputStream>>(
                Util.SerializeCB, Util.DeserializeCB<BondClass<IBonded<Through>>>);

            PolymorphicDeserialization<From, Through, To, CompactBinaryReader<InputStream>>(
                Util.SerializeCB2, Util.DeserializeCB2<BondClass<IBonded<Through>>>);

            PolymorphicDeserialization<From, Through, To, SimpleBinaryReader<InputStream>>(
                Util.SerializeSP, Util.DeserializeSP<BondClass<IBonded<Through>>, BondClass<IBonded<Through>>>);
        }

        // Serialize class with a From field, deserialize as class with IBonded<Through> field
        // which then is serialized (potentially to another protocol) and finally deserialized
        // as class with To field.
        void Passthrough<From, Through, To>(
            Action<BondClass<From, double>, Stream> serialize,
            Func<Stream, BondClass<IBonded<Through>, double>> deserializeThrough,
            Action<BondClass<IBonded<Through>, double>, Stream> serializeThrough,
            Func<Stream, BondClass<To, double>> deserialize)
            where From : class, new()
            where Through : class
            where To : class
        {
            var from = Random.Init<BondClass<From, double>>();

            var stream = new MemoryStream();
            
            serialize(from, stream);
            stream.Position = 0;
            var through = deserializeThrough(stream);

            using (var stream2 = new MemoryStream())
            {
                serializeThrough(through, stream2);
                stream2.Position = 0;

                var to = deserialize(stream2);

                Assert.IsTrue(from.extra.IsEqual(to.extra));
                Assert.AreEqual(from.field, to.field);
            }
        }


        void PassthroughAll<From, Through, To>()
            where From : class, new()
            where Through : class
            where To : class
        {
            Passthrough<From, Through, To>(
                Util.SerializeCB,
                Util.DeserializeCB<BondClass<IBonded<Through>, double>>,
                Util.SerializeCB,
                Util.DeserializeCB<BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeCB2,
                Util.DeserializeCB2<BondClass<IBonded<Through>, double>>,
                Util.SerializeCB2,
                Util.DeserializeCB2<BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeCB,
                Util.DeserializeCB<BondClass<IBonded<Through>, double>>,
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<IBonded<Through>, double>, BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeCB2,
                Util.DeserializeCB2<BondClass<IBonded<Through>, double>>,
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<IBonded<Through>, double>, BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<From, double>, BondClass<IBonded<Through>, double>>,
                Util.SerializeCB,
                Util.DeserializeCB<BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<From, double>, BondClass<IBonded<Through>, double>>,
                Util.SerializeCB2,
                Util.DeserializeCB2<BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<From, double>, BondClass<IBonded<Through>, double>>,
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<IBonded<Through>, double>, BondClass<To, double>>);

            Passthrough<From, Through, To>(
                Util.SerializeSP,
                Util.DeserializeSP<BondClass<From, double>, BondClass<IBonded<Through>, double>>,
                Util.SerializeXmlWithNamespaces,
                Util.DeserializeXml<BondClass<To, double>>);
        }

        [Test]
        public void BondedInterface()
        {
            BondedDeserialize<Nested, Derived>();
            BondedDowncastDeserialize<Nested, Derived>();
            BondedSerialize<Nested, Derived>();
        }

        [Test]
        public void LazyDeserialize()
        {
            LazyDeserializationAll<BasicTypes, BasicTypes>();
            LazyDeserializationAll<BasicTypes, BasicTypesView>();
            LazyDeserializationAll<Derived, Derived>();
            LazyDeserializationAll<Derived, DerivedView>();
        }

        [Test]
        public void NonLazyDeserialize()
        {
            NonLazyDeserializationAll<BasicTypes, BasicTypes>();
            NonLazyDeserializationAll<BasicTypes, BasicTypesView>();
            NonLazyDeserializationAll<Derived, Derived>();
            NonLazyDeserializationAll<Derived, DerivedView>();
        }

        [Test]
        public void PolymorphicDeserialize()
        {
            PolymorphicDeserializationAll<Derived, Nested, Derived>();
            PolymorphicDeserializationAll<Derived, Nested, DerivedView>();
            PolymorphicDeserializationAll<DerivedView, Nested, Derived>();
            PolymorphicDeserializationAll<Derived, EmptyBase, Derived>();
            PolymorphicDeserializationAll<Derived, EmptyBase, Nested>();
            PolymorphicDeserializationAll<DerivedView, EmptyBase, Nested>();
        }

        [Test]
        public void Passthrough()
        {
            PassthroughAll<BasicTypes, BasicTypes, BasicTypes>();
            PassthroughAll<BasicTypes, BasicTypesView, BasicTypes>();
            PassthroughAll<Derived, Nested, Derived>();
            PassthroughAll<Derived, EmptyBase, Derived>();
            PassthroughAll<Derived, EmptyBase, Nested>();
        }

        [Test]
        public void BondedEquals()
        {
            var obj1 = new StructWithBonded();
            var obj2 = new StructWithBonded();
            Assert.IsTrue(Comparer.Equal(obj1, obj2));

            obj1.poly.Add(Bonded<Derived>.Empty);
            Assert.IsFalse(Comparer.Equal(obj1, obj2));

            obj2.poly.Add(Bonded<Derived>.Empty);
            Assert.IsTrue(Comparer.Equal(obj1, obj2));

            obj2.field = new Bonded<Derived>(new Derived());
            Assert.IsFalse(Comparer.Equal(obj1, obj2));
        }

        [Test]
        public void SerializeBonded()
        {
            var obj = new StructWithBonded();

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
            obj = Util.DeserializeCB<StructWithBonded>(stream);

            Assert.IsTrue(Comparer.Equal(field, obj.field.Deserialize<Derived>()));
            Assert.IsTrue(Comparer.Equal(poly0, obj.poly[0].Deserialize<EmptyBase>()));
            Assert.IsTrue(Comparer.Equal(poly1, obj.poly[1].Deserialize()));
            Assert.IsTrue(Comparer.Equal(poly2, obj.poly[2].Deserialize<Derived>()));

            stream.SetLength(0);
            Util.SerializeCB(obj, stream);
            stream.Position = 0;
            obj = Util.DeserializeCB<StructWithBonded>(stream);

            Assert.IsTrue(Comparer.Equal(field, obj.field.Deserialize<Derived>()));
            Assert.IsTrue(Comparer.Equal(poly0, obj.poly[0].Deserialize<EmptyBase>()));
            Assert.IsTrue(Comparer.Equal(poly1, obj.poly[1].Deserialize()));
            Assert.IsTrue(Comparer.Equal(poly2, obj.poly[2].Deserialize<Derived>()));
        }

        [Test]
        public void SerializeBondedCB2Multiple()
        {
            var obj = new StructWithBonded();

            var stream = new MemoryStream();
            stream.SetLength(0);
            Util.SerializeCB2Multiple(obj, stream, 2);
        }
    }
}
