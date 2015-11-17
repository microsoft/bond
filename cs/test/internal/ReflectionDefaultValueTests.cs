namespace InternalTest
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using NUnit.Framework;
    using Bond;
    using Bond.Tag;

    [TestFixture]
    public class ReflectionDefaultValueTests
    {
        [Test]
        public void ReflectionDefaultAttribute_Absent_On_Interface()
        {
            Assert.AreEqual(0, GetDefaultValue<IFoo>("IntNoDefault"));
            Assert.AreEqual(0.0, GetDefaultValue<IFoo>("DoubleNoDefault"));
            Assert.AreEqual(0.0f, GetDefaultValue<IFoo>("FloatNoDefault"));
            Assert.AreEqual(false, GetDefaultValue<IFoo>("BoolNoDefault"));
        }

        [Test]
        public void ReflectionDefaultAttribute_Non_Null_On_Nullable_Property()
        {
            Assert.Throws<InvalidOperationException>(() => GetDefaultValue<IFoo>("NullableNonNullDefault"));
        }

        [Test]
        public void ReflectionDefaultAttribute_On_Class()
        {
            Assert.Throws<InvalidOperationException>(() => GetDefaultValue<Foo>("HasDefault"));
        }

        [Test]
        public void ReflectionDefaultAttribute_On_Interface()
        {
            Assert.AreEqual(7, GetDefaultValue<IFoo>("IntField"));
            Assert.AreEqual(0, GetDefaultValue<IFoo>("IntFieldZero"));
            Assert.AreEqual(true, GetDefaultValue<IFoo>("BoolField"));
            Assert.AreEqual(19.143, GetDefaultValue<IFoo>("DoubleField"));
            Assert.AreEqual("Hello", GetDefaultValue<IFoo>("StringField"));
            Assert.AreEqual(string.Empty, GetDefaultValue<IFoo>("EmptyStringField"));
            Assert.IsNull(GetDefaultValue<IFoo>("NullStringField"));
            Assert.IsNull(GetDefaultValue<IFoo>("NullableNoDefault"));
            Assert.IsNull(GetDefaultValue<IFoo>("NullableDefault"));
            Assert.IsNotNull(GetDefaultValue<IFoo>("EmptyStruct"));
            Assert.IsNotNull(GetDefaultValue<IFoo>("EmptyList"));
            Assert.IsNull(GetDefaultValue<IFoo>("NullableList"));
            Assert.IsNull(GetDefaultValue<IFoo>("NothingList"));
            Assert.IsNull(GetDefaultValue<IFoo>("NullableBlob"));
            Assert.IsNull(GetDefaultValue<IFoo>("NothingBlob"));
        }
        
        static object GetDefaultValue<T>(string name)
        {
            return GetMember<T>(name).GetDefaultValue();
        }

        static ISchemaField GetMember<T>(string name)
        {
            return typeof(T).GetSchemaFields().Single(f => f.Name.Equals(name, StringComparison.Ordinal));
        }

        [Schema]
        interface IFoo
        {
            [Id(1), Default(7)]
            int IntField { get; set; }

            [Id(2), Default(0)]
            int IntFieldZero { get; set; }
            
            [Id(3), Default(true)]
            bool BoolField { get; set; }

            [Id(4), Default(19.143)]
            double DoubleField { get; set; }

            [Id(5), Default("Hello")]
            string StringField { get; set; }

            [Id(6), Default("")]
            string EmptyStringField { get; set; }

            [Id(7), Default(null)]
            string NullStringField { get; set; }

            [Id(8)]
            int IntNoDefault { get; set; }

            [Id(18)]
            float FloatNoDefault { get; set; }

            [Id(19)]
            double DoubleNoDefault { get; set; }

            [Id(20)]
            bool BoolNoDefault { get; set; }

            [Id(9), Type(typeof(nullable<IFoo>))]
            IFoo NullableNoDefault { get; set; }

            [Id(10), Type(typeof(nullable<IFoo>)), Default(null)]
            IFoo NullableDefault { get; set; }

            [Id(11), Type(typeof(nullable<IFoo>)), Default("foo")]
            IFoo NullableNonNullDefault { get; set; }

            [Id(12)]
            IFoo EmptyStruct { get; set; }

            [Id(13)]
            IList<string> EmptyList { get; set; }

            [Id(14), Type(typeof(nullable<IList<string>>))]
            IList<string> NullableList { get; set; }

            [Id(15), Default(null)]
            IList<string> NothingList { get; set; }

            [Id(16), Type(typeof(nullable<blob>))]
            ArraySegment<byte> NullableBlob { get; set; }

            [Id(17), Default(null)]
            ArraySegment<byte> NothingBlob { get; set; }
        }

        [Schema]
        class Foo
        {
            [Id(1), Default(7)]
            int HasDefault { get; set; }
        }
    }
}
