namespace InternalTest
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using Bond;
    using NUnit.Framework;
    using Bond.Tag;

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
        public void GenericSchemaType()
        {
            GenericSchemaStructTest<int>();
            GenericSchemaStructTest<float>();
            GenericSchemaClassTest<BasicTypes>();
            GenericSchemaClassTest<List<string>>();
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
    }
}
