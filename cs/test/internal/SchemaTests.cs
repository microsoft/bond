namespace InternalTest
{
    using System;
    using Bond;
    using NUnit.Framework;

    [TestFixture]
    public class SchemaTests
    {
        [Test]
        public void Schema_Metadata()
        {
            Assert.AreEqual("BasicTypes", Schema<BasicTypes>.Metadata.name);
            Assert.AreEqual("InternalTest.BasicTypes", Schema<BasicTypes>.Metadata.qualified_name);
            Assert.AreEqual(2, Schema<BasicTypes>.Metadata.attributes.Count);
            Assert.AreEqual("foo", Schema<BasicTypes>.Metadata.attributes["Foo"]);
            Assert.AreEqual("bar", Schema<BasicTypes>.Metadata.attributes["Bar"]);
            Assert.AreEqual("Generic<blob>", Schema<Names>.RuntimeSchema.SchemaDef.structs[1].metadata.name);
            Assert.AreEqual("Generic<wstring>", Schema<Names>.RuntimeSchema.SchemaDef.structs[2].metadata.name);
            Assert.AreEqual("InternalTest.Generic<nullable<InternalTest.BasicTypes>>", Schema<Names>.RuntimeSchema.SchemaDef.structs[3].metadata.qualified_name);
            Assert.AreEqual("Generic<map<int8, InternalTest.EnumType1>>", Schema<Names>.RuntimeSchema.SchemaDef.structs[4].metadata.name);
        }

        [Test]
        public void Schema_Fields()
        {
            Assert.AreEqual("_bool", Schema<BasicTypes>.Fields[0].name);
            Assert.AreEqual("Boolean", Schema<BasicTypes>.Fields[0].attributes["Name"]);
            Assert.AreEqual(Modifier.Optional, Schema<BasicTypes>.Fields[0].modifier);
            Assert.AreEqual("_enum1", Schema<BasicTypes>.Fields[13].name);
            Assert.AreEqual(5, Schema<BasicTypes>.Fields[13].default_value.int_value);
            Assert.AreEqual(Modifier.Required, Schema<Required>.Fields[0].modifier);
        }

        [Test]
        public void Schema_DefaultValues()
        {
            Assert.AreEqual(1ul, Schema<StructWithDefaults>.Fields[0].default_value.uint_value);
            Assert.AreEqual(0ul, Schema<StructWithDefaults>.Fields[1].default_value.uint_value);
            Assert.AreEqual(0ul, Schema<StructWithDefaults>.Fields[2].default_value.uint_value);

            Assert.AreEqual("default string value", Schema<StructWithDefaults>.Fields[3].default_value.string_value);
            Assert.AreEqual(String.Empty, Schema<StructWithDefaults>.Fields[4].default_value.string_value);

            Assert.AreEqual(-127, Schema<StructWithDefaults>.Fields[5].default_value.int_value);
            Assert.AreEqual(0, Schema<StructWithDefaults>.Fields[6].default_value.int_value);

            Assert.AreEqual(-32767, Schema<StructWithDefaults>.Fields[7].default_value.int_value);
            Assert.AreEqual(0, Schema<StructWithDefaults>.Fields[8].default_value.int_value);

            Assert.AreEqual(0, Schema<StructWithDefaults>.Fields[9].default_value.int_value);
            Assert.AreEqual(2147483647, Schema<StructWithDefaults>.Fields[10].default_value.int_value);

            Assert.AreEqual(0, Schema<StructWithDefaults>.Fields[11].default_value.int_value);
            Assert.AreEqual(9223372036854775807, Schema<StructWithDefaults>.Fields[12].default_value.int_value);

            Assert.AreEqual(255ul, Schema<StructWithDefaults>.Fields[13].default_value.uint_value);
            Assert.AreEqual(0ul, Schema<StructWithDefaults>.Fields[14].default_value.uint_value);

            Assert.AreEqual(65535ul, Schema<StructWithDefaults>.Fields[15].default_value.uint_value);
            Assert.AreEqual(0ul, Schema<StructWithDefaults>.Fields[16].default_value.uint_value);

            Assert.AreEqual(0ul, Schema<StructWithDefaults>.Fields[17].default_value.uint_value);
            Assert.AreEqual(4294967295ul, Schema<StructWithDefaults>.Fields[18].default_value.uint_value);

            Assert.AreEqual(0ul, Schema<StructWithDefaults>.Fields[19].default_value.uint_value);
            Assert.AreEqual(0xFFFFFFFFFFFFFFFFul, Schema<StructWithDefaults>.Fields[20].default_value.uint_value);

            Assert.AreEqual(0.0, Schema<StructWithDefaults>.Fields[21].default_value.double_value);
            Assert.AreEqual(-123.4567890, Schema<StructWithDefaults>.Fields[22].default_value.double_value);
            Assert.AreEqual(-0.0, Schema<StructWithDefaults>.Fields[23].default_value.double_value);

            Assert.AreEqual(0.0, Schema<StructWithDefaults>.Fields[24].default_value.double_value);
            Assert.AreEqual(2.71828183f, Schema<StructWithDefaults>.Fields[25].default_value.double_value, 0.0000005);
            Assert.AreEqual(-0.0, Schema<StructWithDefaults>.Fields[26].default_value.double_value);

            Assert.AreEqual((long)EnumType1.EnumValue1, Schema<StructWithDefaults>.Fields[27].default_value.int_value);
            Assert.AreEqual((long)EnumType1.EnumValue3, Schema<StructWithDefaults>.Fields[28].default_value.int_value);

            Assert.AreEqual("default wstring value", Schema<StructWithDefaults>.Fields[29].default_value.wstring_value);
            Assert.AreEqual(String.Empty, Schema<StructWithDefaults>.Fields[30].default_value.string_value);
        }
    }
}
