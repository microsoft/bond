namespace InternalTest
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using Bond;
    using Bond.Internal.Reflection;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using Bond.Tag;
    using NUnit.Framework;

    [TestFixture]
    public class ReflectionTests
    {
        [Test]
        public void Reflection_Interface_Base()
        {
            Assert.AreEqual(typeof(IBase), typeof(ISub).GetBaseSchemaType());
        }

        [Test]
        public void Reflection_Interface_Chain_Base()
        {
            Assert.AreEqual(typeof(ISub), typeof(ISubSub).GetBaseSchemaType());
        }

        [Test]
        public void Reflection_GenericSchemaType()
        {
            GenericSchemaStructTest<int>();
            GenericSchemaStructTest<float>();
            GenericSchemaClassTest<BasicTypes>();
            GenericSchemaClassTest<List<string>>();
        }

        [Test]
        public void Reflection_FindMethodFromObject()
        {
            Assert.AreEqual("ReadStructBegin", ReflectionExtensions.FindMethod(typeof(ReaderA), "ReadStructBegin", new Type[0]).Name);
            Assert.AreEqual(typeof(ReaderA), ReflectionExtensions.FindMethod(typeof(ReaderA), "ReadStructBegin", new Type[0]).DeclaringType);
        }

        [Test]
        public void Reflection_FindMethodFromInterface()
        {
            Assert.AreEqual("ReadStructBegin", ReflectionExtensions.FindMethod(typeof(IReaderA), "ReadStructBegin", new Type[0]).Name);
            Assert.AreEqual(typeof(IReaderA), ReflectionExtensions.FindMethod(typeof(IReaderA), "ReadStructBegin", new Type[0]).DeclaringType);
        }

        [Test]
        public void Reflection_MultipleMethodsImplementedException()
        {
            Assert.That(() => ReflectionExtensions.FindMethod(typeof(IReaderAB), "ReadStructBegin", new Type[0]),
                Throws.TypeOf<System.Reflection.AmbiguousMatchException>()
                     .With.Message.Contains("FindMethod found more than one matching method"));
        }

        // We test on the SchemaFields instead of the RuntimeSchema, because, for now, the list sub
        // type is not part of Bond.TypeDef
        [Test]
        public void Reflection_DifferentiateBetweenListAndNullable()
        {
            var schemaFields = typeof(ListVsNullable).GetSchemaFields();

            foreach (var field in schemaFields)
            {
                ListSubType fieldListSubType = field.GetSchemaType().GetBondListDataType();

                switch (field.Name)
                {
                    case "nullableInt":
                        Assert.AreEqual(ListSubType.NULLABLE_SUBTYPE, fieldListSubType);
                        break;

                    case "vectorInt":
                        Assert.AreEqual(ListSubType.NO_SUBTYPE, fieldListSubType);
                        break;

                    case "listInt":
                        Assert.AreEqual(ListSubType.NO_SUBTYPE, fieldListSubType);
                        break;

                    case "blobData":
                        Assert.AreEqual(ListSubType.BLOB_SUBTYPE, fieldListSubType);
                        break;

                    default:
                        Assert.Fail("Unexpected field '{0}'", field.Name);
                        break;
                }
            }
        }

        [Test]
        public void Reflection_IsBonded()
        {
            Assert.IsTrue(Reflection.IsBonded(typeof(Bonded<>)));
            Assert.IsTrue(Reflection.IsBonded(typeof(Bonded<BasicTypes>)));
            Assert.IsTrue(Reflection.IsBonded(typeof(BondedVoid<>)));
            Assert.IsTrue(Reflection.IsBonded(typeof(BondedVoid<CompactBinaryReader<InputBuffer>>)));
            Assert.IsTrue(Reflection.IsBonded(typeof(CustomBonded<>)));
            Assert.IsTrue(Reflection.IsBonded(typeof(CustomBonded<BasicTypes>)));

            Assert.IsFalse(Reflection.IsBonded(typeof(int)));
            Assert.IsFalse(Reflection.IsBonded(typeof(BasicTypes)));
        }

        // We test on the SchemaFields instead of the RuntimeSchema, because, for now, the list sub
        // type is not part of Bond.TypeDef
        [Test]
        public void Reflection_EnsureUnknownSeqIDLType()
        {
            var schemaFields = typeof(BasicTypes).GetSchemaFields();

            foreach (var field in schemaFields)
            {
                ListSubType fieldListSubType = field.GetSchemaType().GetBondListDataType();
                Assert.AreEqual(ListSubType.NO_SUBTYPE, fieldListSubType, "Failed on field '{0}'", field.Name);
            }
        }

        static Type GetFieldSchemaTypeClass<T>(string name)
        {
            return typeof(Class<T>).GetSchemaFields().Single(
                f => f.Name.Equals(name, StringComparison.Ordinal)).GetSchemaType();
        }

        void GenericSchemaClassTest<T>()
        {
            Assert.AreEqual(typeof(nullable<T>), GetFieldSchemaTypeClass<T>("x1"));
            Assert.AreEqual(typeof(List<T>), GetFieldSchemaTypeClass<T>("x2"));
            Assert.AreEqual(typeof(nullable<List<T>>), GetFieldSchemaTypeClass<T>("x3"));
            Assert.AreEqual(typeof(List<nullable<T>>), GetFieldSchemaTypeClass<T>("x4"));
            Assert.AreEqual(typeof(List<nullable<SortedSet<nullable<T>>>>), GetFieldSchemaTypeClass<T>("x5"));
            Assert.AreEqual(typeof(Dictionary<nullable<wstring>, nullable<T>>), GetFieldSchemaTypeClass<T>("x6"));
        }

        static Type GetFieldSchemaTypeStruct<T>(string name) where T : struct
        {
            return typeof(Struct<T>).GetSchemaFields().Single(
                f => f.Name.Equals(name, StringComparison.Ordinal)).GetSchemaType();
        }

        void GenericSchemaStructTest<T>() where T : struct
        {
            Assert.AreEqual(typeof(nullable<T>), GetFieldSchemaTypeStruct<T>("x1"));
            Assert.AreEqual(typeof(List<T>), GetFieldSchemaTypeStruct<T>("x2"));
            Assert.AreEqual(typeof(nullable<List<T>>), GetFieldSchemaTypeStruct<T>("x3"));
            Assert.AreEqual(typeof(List<nullable<T>>), GetFieldSchemaTypeStruct<T>("x4"));
            Assert.AreEqual(typeof(List<nullable<SortedSet<nullable<T>>>>), GetFieldSchemaTypeStruct<T>("x5"));
            Assert.AreEqual(typeof(Dictionary<nullable<wstring>, nullable<T>>), GetFieldSchemaTypeStruct<T>("x6"));
        }

        [Schema]
        class Class<T>
        {
            [Id(0), Type(typeof(nullable<structT>))]
            public T x1 = default(T);

            [Id(1), Type(typeof(List<classT>))]
            public List<T> x2 = null;

            [Id(2), Type(typeof(nullable<List<structT>>))]
            public IList<T> x3 = null;

            [Id(3), Type(typeof(List<nullable<classT>>))]
            public List<T> x4 = null;

            [Id(4), Type(typeof(List<nullable<SortedSet<nullable<classT>>>>))]
            public List<ISet<T>> x5 = null;

            [Id(5), Type(typeof(Dictionary<nullable<wstring>, nullable<classT>>))]
            public Dictionary<string, T> x6 = null;
        }

        class Struct<T> where T : struct
        {
            [Id(0), Type(typeof(nullable<structT>))]
            public T? x1 = default(T);

            [Id(1), Type(typeof(List<classT>))]
            public List<T> x2 = null;

            [Id(2), Type(typeof(nullable<List<structT>>))]
            public IList<T> x3 = null;

            [Id(3), Type(typeof(List<nullable<classT>>))]
            public List<T?> x4 = null;

            [Id(4), Type(typeof(List<nullable<SortedSet<nullable<classT>>>>))]
            public List<ISet<T?>> x5 = null;

            [Id(5), Type(typeof(Dictionary<nullable<wstring>, nullable<classT>>))]
            public Dictionary<string, T?> x6 = null;
        }

        [Schema]
        interface IBase
        {
        }

        [Schema]
        interface ISub : IBase
        {
        }

        [Schema]
        // ReSharper disable once RedundantExtendsListEntry
        interface ISubSub : IBase, ISub
        {
        }

        interface IReaderA
        {
            void ReadStructBegin();
        }

        interface IReaderB
        {
            void ReadStructBegin();
        }

        interface IReaderAB : IReaderA, IReaderB
        {
        }

        class ReaderA : IReaderA
        {
            public void ReadStructBegin()
            {
                throw new NotImplementedException();
            }
        }

        class CustomBonded<T> : IBonded
        {
            readonly IBonded<T> _instance;

            public CustomBonded(IBonded<T> instance)
            {
                _instance = instance;
            }

            public void Serialize<W>(W writer)
            {
                _instance.Serialize<W>(writer);
            }

            public U Deserialize<U>()
            {
                return _instance.Deserialize<U>();
            }

            public IBonded<U> Convert<U>()
            {
                return _instance.Convert<U>();
            }
        }

    }
}
