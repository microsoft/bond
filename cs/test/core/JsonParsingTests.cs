namespace UnitTest
{
    using System;
    using System.IO;
    using System.Text;
    using Bond;
    using Bond.Protocols;
    using Newtonsoft.Json;
    using NUnit.Framework;

    [TestFixture]
    public class JsonParsingTests
    {
        [Test]
        public void JsonParsing_EmptyStruct()
        {
            const string json = @"{ }";
            var target = ParseJson<BasicTypes>(json);
            Assert.IsNotNull(target);
        }

        [Test]
        public void JsonParsing_Scalar_Field_Types_Read_Successfully()
        {
            // the purpose of this test is to ensure correct handling of scalar types (this
            // ensure we cast correctly the boxed value returned from the inner JSON reader).
            const string json = @"
{
 ""_str"" : ""foo"",
 ""_wstr"" : ""bar"",
 ""_int8"": -7,
 ""_int16"": -17,
 ""_int32"": -117,
 ""_int64"": -1117,
 ""_uint8"": 7,
 ""_uint16"": 17,
 ""_uint32"": 117,
 ""_uint64"": 1117,
 ""_bool"": true,
 ""_float"": 5,
 ""_double"": -89.14,
 ""_enum1"" : 10,
 ""dt"" : 123456
}";

            // TODO: enums should be serialized as names and not as numbers (when this is fixed, update this test)
            var target = ParseJson<BasicTypes>(json);
            Assert.AreEqual("foo", target._str);
            Assert.AreEqual("bar", target._wstr);
            Assert.AreEqual(-7, target._int8);
            Assert.AreEqual(-17, target._int16);
            Assert.AreEqual(-117, target._int32);
            Assert.AreEqual(-1117, target._int64);
            Assert.AreEqual(7, target._uint8);
            Assert.AreEqual(17, target._uint16);
            Assert.AreEqual(117u, target._uint32);
            Assert.AreEqual(1117u, target._uint64);
            Assert.AreEqual(true, target._bool);

            // JsonReader reads the input as double.
            // casting from double to float leads to floating-point in accuracy.
            Assert.IsTrue(Math.Abs(target._float - 5) < 0.000001);
            
            Assert.AreEqual(-89.14, target._double);
            Assert.AreEqual(EnumType1.EnumValue2, target._enum1);
            Assert.AreEqual(123456, target.dt.Ticks);
        }

        [Test]
        public void JsonParsing_BasicModel()
        {
            const string json = @"
{
 ""_str"": ""Hello"",
 ""_bool"": true,
 ""_double"":13.2,
 ""_int32"":17
}";

            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
            Assert.AreEqual(17, target._int32);
        }

        [Test]
        public void JsonParsing_InvalidScalar()
        {
            const string json = @"
{
    ""_double"": 1a3.2
}";

            try
            {
                ParseJson<BasicTypes>(json);
                Assert.Fail("Deserialization succeeded even though \"_double\" is invalid");
            }
            catch (JsonReaderException)
            {
                // OK
            }
        }

        [Test]
        public void JsonParsing_BasicModelWithUnknownField()
        {
            const string json = @"
{
    ""_str"":""Hello"",
    ""foo"" : """",
    ""_bool"": true,
    ""_double"":13.2
}";

            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void JsonParsing_BasicModelWithUnknownNestedField()
        {
            const string json = @"
{
    ""_str"":""Hello"",
    ""foo"" : { ""a"": {}, ""b"": [1,2,3]},
    ""_bool"": true,
    ""_double"":13.2
}";
            
            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void JsonParsing_BasicModelWithUnknownListField()
        {
            const string json = @"
{
    ""_str"":""Hello"",
    ""foo"" : [ ""a"", {}, [1, 2, null, ""bar""]],
    ""_bool"": true,
    ""_double"":13.2
}";
        
            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void JsonParsing_BasicModelWithUnknownNullElement()
        {
            const string json = @"
{
    ""_str"":""Hello"",
    ""foo"" : null,
    ""_bool"": true,
    ""_double"":13.2
}";

            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual("Hello", target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void JsonParsing_MultilineString()
        {
            const string json = @"
{ ""_str"": ""Hello
World""
}";

            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual(@"Hello
World", target._str);
        }

        [Test]
        public void JsonParsing_WhitespaceString()
        {
            const string json = @"
{ ""_str"":"" ""}";

            var target = ParseJson<BasicTypes>(json);

            Assert.AreEqual(" ", target._str);
        }

        [Test]
        public void JsonParsing_EmptyStringField()
        {
            const string json = @"
{
    ""_str"":"""",
    ""_wstr"":"""",
    ""_bool"":true,
    ""_double"":13.2,
    ""unexpectedObject"":{ ""withNesting"": {}}
}";

            var target = ParseJson<BasicTypes>(json);

            Assert.IsEmpty(target._str);
            Assert.IsTrue(target._bool);
            Assert.AreEqual(13.2, target._double);
        }

        [Test]
        public void JsonParsing_Fail_On_Null_Field()
        {
            const string json = @"{""_bool"":null}";

            try
            {
                ParseJson<BasicTypes>(json);
                Assert.Fail("Deserialize did not throw an exception even though _bool field was null.");
            }
            catch (InvalidDataException)
            {
            }
        }

        [Test]
        public void JsonParsing_Nested()
        {
            const string json = @"
{
  ""basic"":
    {
      ""_str"":""Hello"",
      ""_bool"":true,
     ""_double"":13.2,
     ""_float"" : 8.01
    },
  ""nested"":
    {
      ""basic1"":{""_int32"":17},
      ""basic2"":{""_int16"":-101}
    }
}";

            var target = ParseJson<Nested>(json);

            Assert.AreEqual("Hello", target.basic._str);
            Assert.IsTrue(target.basic._bool);
            Assert.AreEqual(13.2, target.basic._double);
            Assert.AreEqual(8.01f, target.basic._float);
            Assert.AreEqual(17, target.nested.basic1._int32);
            Assert.AreEqual(-101, target.nested.basic2._int16);
        }

        [Test]
        public void JsonParsing_NullNested()
        {
            const string json = @"
{
   ""root"" : null
}";

            var target = ParseJson<Tree>(json);

            Assert.IsNull(target.root);
        }
        
        [Test]
        public void JsonParsing_RequiredFieldEmptyRoot()
        {
            const string json = "{}";

            try
            {
                ParseJson<Required>(json);
                Assert.Fail("Deserialization succeeded even though required field is missing");
            }
            catch (InvalidDataException ex)
            {
                Assert.AreEqual("Required field UnitTest.Required.x missing", ex.Message);
            }
        }

        [Test]
        public void JsonParsing_MissingRequiredScalarField()
        {
            const string json = @"{ ""y"" : {} }";

            try
            {
                ParseJson<Required>(json);
                Assert.Fail("Deserialization succeeded even though required field is missing");
            }
            catch (InvalidDataException ex)
            {
                Assert.AreEqual("Required field UnitTest.Required.x missing", ex.Message);
            }
        }

        [Test]
        public void JsonParsing_MissingRequiredStructField()
        {
            const string json = @"{ ""RequiredX"" : 5 }";
   
            try
            {
                ParseJson<Required>(json); 
                Assert.Fail("Deserialization succeeded even though required field is missing");
            }
            catch (InvalidDataException ex)
            {
                Assert.AreEqual("Required field UnitTest.Required.y missing", ex.Message);
            }
        }

        [Test]
        public void JsonParsing_MissingRequiredInBase()
        {
            const string json = @"{ ""bar"" : ""a"", ""foo"": 7, ""bla"" : 4.3, ""flag"": true, ""y"": {} }";

            try
            {
                ParseJson<RequiredInBase>(json);
                Assert.Fail("Deserialization succeeded even though required field is missing");
            }
            catch (InvalidDataException ex)
            {
                Assert.AreEqual("Required field UnitTest.Required.x missing", ex.Message);
            }
        }

        [Test]
        public void JsonParsing_MissingRequiredInDerived()
        {
            const string json = @"{ ""bar"" : ""a"", ""x"": 8, ""bla"" : 4.3, ""flag"": true, ""y"": {} }";

            try
            {
                ParseJson<DerivedRequired>(json); 
                Assert.Fail("Deserialization succeeded even though required field is missing");
            }
            catch (InvalidDataException ex)
            {
                Assert.AreEqual("Required field UnitTest.DerivedRequired.foo missing", ex.Message);
            }
        }

        [Test]
        public void JsonParsing_Containers()
        {
            const string json = @"
{
  ""strings"": [ ""foo"", ""bar"" ],
  ""basics"":
  [
    { ""_str"": ""first"" },
    { ""_str"": ""second"" },
    { ""_str"": ""third"" },
  ],
  ""numbers"": [1, ""one"", 5, ""five"" ]
}";
            var target = ParseJson<SimpleContainers>(json);

            Assert.AreEqual(2, target.strings.Count);
            Assert.AreEqual("foo", target.strings[0]);
            Assert.AreEqual("bar", target.strings[1]);
            Assert.AreEqual(3, target.basics.Count);
            Assert.AreEqual("first", target.basics[0]._str);
            Assert.AreEqual("second", target.basics[1]._str);
            Assert.AreEqual("third", target.basics[2]._str);
            Assert.AreEqual(2, target.numbers.Count);
            Assert.AreEqual("one", target.numbers[1]);
            Assert.AreEqual("five", target.numbers[5]);
        }

        [Test]
        public void JsonParsing_EmptyContainers()
        {
            const string json = @"
{
  ""basics"": [
    { ""_str"":""first"" },
    { ""_str"":""second"" },
    { ""_str"":""third"" }
  ],
  ""strings"":[]
}";
            var target = ParseJson<SimpleContainers>(json);

            Assert.AreEqual(0, target.strings.Count);
            Assert.AreEqual(3, target.basics.Count);
            Assert.AreEqual("first", target.basics[0]._str);
            Assert.AreEqual("second", target.basics[1]._str);
            Assert.AreEqual("third", target.basics[2]._str);
        }

        [Test]
        public void JsonParsing_NestedContainers()
        {
            const string json = @"
{
  ""vvb"":
  [
    [ 7, 13 ],
    [ 5 ],
    [ 9 ]
  ]
}";

            var target = ParseJson<NestedContainers>(json);

            Assert.AreEqual(3, target.vvb.Count);
            Assert.AreEqual(2, target.vvb[0].Count);
            Assert.AreEqual(1, target.vvb[1].Count);
            Assert.AreEqual(1, target.vvb[2].Count);
            Assert.AreEqual((uint)7, target.vvb[0][0]);
            Assert.AreEqual((uint)13, target.vvb[0][1]);
            Assert.AreEqual((uint)5, target.vvb[1][0]);
            Assert.AreEqual((uint)9, target.vvb[2][0]);
        }

        [Test]
        public void JsonParsing_EmptyNestedContainers()
        {
            const string json = @"
{
  ""vvb"":
  [
    [ 7, 13 ],
    [],
    [ 9 ],
    []
  ]
}";

            var target = ParseJson<NestedContainers>(json);

            Assert.AreEqual(4, target.vvb.Count);
            Assert.AreEqual(2, target.vvb[0].Count);
            Assert.AreEqual(0, target.vvb[1].Count);
            Assert.AreEqual(1, target.vvb[2].Count);
            Assert.AreEqual(0, target.vvb[3].Count);
            Assert.AreEqual((uint)7, target.vvb[0][0]);
            Assert.AreEqual((uint)13, target.vvb[0][1]);
            Assert.AreEqual((uint)9, target.vvb[2][0]);
        }

        [Test]
        public void JsonParsing_EmptyStructsInContainer()
        {
            const string json = @"
{
  ""basics"": [ {}, {}, { ""_str"":""Something"" }, {} ],
  ""strings"": [ ""Hello"" ]
}";

            var target = ParseJson<SimpleContainers>(json);

            Assert.AreEqual(4, target.basics.Count);
            Assert.AreEqual("Something", target.basics[2]._str);
            Assert.AreEqual(1, target.strings.Count);
            Assert.AreEqual("Hello", target.strings[0]);
        }

        [Test]
        public void JsonParsing_Recursive()
        {
            const string json = @"
{
    ""root"":
    [{
        ""left"":
        [{
            ""left"":null,
            ""right"":[],
            ""value"":
            {
                ""_str"":""Hello""
            }
        }],
        ""right"":null,
        ""value"":
        {
            ""_double"":13.2
        }
    }]
}";

            var target = ParseJson<Tree>(json);

            Assert.AreEqual("Hello", target.root.left.value._str);
            Assert.AreEqual(13.2, target.root.value._double);
            Assert.AreEqual(null, target.root.left.left);
            Assert.AreEqual(null, target.root.left.right);
            Assert.AreEqual(null, target.root.right);
        }

        [Test]
        public void JsonParsing_Recursive_Unwrapped_Nullable()
        {
            const string json = @"
{
    ""root"": {}
}";

            try
            {
                ParseJson<Tree>(json);
                Assert.Fail(
                    "Parsing JSON didn't throw an exeception even though nullable struct was not wrapped in [ ].");
            }
            catch (InvalidDataException ex)
            {
                Assert.IsTrue(ex.Message.Contains("Expected JSON array"));
            }
        }

        [Test]
        public void JsonParsing_Read_Null_From_Null_Token()
        {
            const string json = @"{ ""basic"": null, ""nested"": null}";
            var target = ParseJson<NullableStruct>(json);
            Assert.IsNull(target.basic);
            Assert.IsNull(target.nested);
        }

        [Test]
        public void JsonParsing_Read_Null_From_Empty_List()
        {
            const string json = @"{ ""basic"": [], ""nested"": []}";
            var target = ParseJson<NullableStruct>(json);
            Assert.IsNull(target.basic);
            Assert.IsNull(target.nested);
        }

        [Test]
        public void JsonParsing_Read_Null_Nullable_List_From_Null()
        {
            const string json = @"{ ""_bool"": null }";
            var target = ParseJson<NullableLists>(json);
            Assert.IsNull(target._bool);
        }

        [Test]
        public void JsonParsing_Read_Null_Nullable_List_From_Array()
        {
            const string json = @"{ ""_bool"": [] }";
            var target = ParseJson<NullableLists>(json);
            Assert.IsNull(target._bool);
        }

        [Test]
        public void JsonParsing_Read_Empty_Nullable_List_From_Array()
        {
            const string json = @"{ ""_bool"": [[]] }";
            var target = ParseJson<NullableVectors>(json);
            Assert.IsNotNull(target._bool);
            Assert.AreEqual(0, target._bool.Count);
        }

        [Test]
        public void JsonParsing_Read_NonEmpty_Nullable_List_From_Array()
        {
            const string json = @"{ ""_int32"": [[4, 5]] }";
            var target = ParseJson<NullableVectors>(json);
            Assert.IsNotNull(target._int32);
            Assert.AreEqual(2, target._int32.Count);
            Assert.AreEqual(4, target._int32[0]);
            Assert.AreEqual(5, target._int32[1]);
        }

        [Test]
        public void JsonParsing_Cannot_Read_NonEmpty_Nullable_List_From_Array_Unwrapped()
        {
            const string json = @"{ ""_int32"": [4, 5] }";

            try
            {
                ParseJson<NullableVectors>(json);
                Assert.Fail("Parse didn't throw even though input contained nullable list without wrapper.");
            }
            catch (InvalidDataException ex)
            {
                Assert.IsTrue(ex.Message.Contains("Expected JSON array"));
            }
        }

        [Test]
        public void JsonParsing_Read_NonNull_Nullable_From_List()
        {
            const string json = @"{ ""basic"": [ { ""_str"" : ""foo""} ] }";
            var target = ParseJson<NullableStruct>(json);
            Assert.IsNotNull(target.basic);
            Assert.AreEqual("foo", target.basic._str);
        }

        [Test]
        public void JsonParsing_Read_NonNull_Nullable()
        {
            const string json = @"{ ""basic"": { ""_str"" : ""foo""} }";
            
            // unwrapped nullable not allowed
            try
            {
                ParseJson<NullableStruct>(json);
                Assert.Fail("Expected exception");
            }
            catch (InvalidDataException ex)
            {
                Assert.IsTrue(ex.Message.Contains("Expected JSON array"));
            }
        }

        [Test]
        public void JsonParsing_Read_Null_As_Nothing()
        {
            const string json = @"{ ""_bool"": null }";

            try
            {
                ParseJson<Nothing>(json);
                Assert.Fail("Deserialize did not throw an exception even though _bool field was null.");
            }
            catch (InvalidDataException ex)
            {
                Assert.IsTrue(ex.Message.Contains("expected JSON token of type Boolean"));
            }
        }

        [Test]
        public void JsonParsing_Read_Field_With_Nothing_Default()
        {
            const string json = @"{ ""_int32"": 17 }";

            var target = ParseJson<Nothing>(json);
            Assert.IsTrue(target._int32.HasValue); 
            Assert.AreEqual(17, target._int32.Value);
        }

        [Test]
        public void JsonParsing_Cannot_Read_Field_With_Nothing_Default_From_Array()
        {
            const string json = @"{ ""_int32"": [ 17 ] }";

            try
            {
                ParseJson<Nothing>(json);
                Assert.Fail("Parsing succeeded even though non-nullable int field was wrapped in an array.");
            }
            catch (InvalidDataException ex)
            {
                Assert.IsTrue(ex.Message.Contains("Integer"));
            }
        }

        [Test]
        public void JsonParsing_Blobs()
        {
            const string json = @"
{
    ""b"": [ 4, 5, 6],
    ""nb"": [[ 10, 11 ]]
}";

            var target = ParseJson<StructWithBlobs>(json);

            Assert.AreEqual(3, target.b.Count);
            Assert.AreEqual(4, target.b.Array[target.b.Offset]);
            Assert.AreEqual(5, target.b.Array[target.b.Offset + 1]);
            Assert.AreEqual(6, target.b.Array[target.b.Offset + 2]);
            
            Assert.AreEqual(2, target.nb.Count);
            Assert.AreEqual(10, target.nb.Array[target.nb.Offset]);
            Assert.AreEqual(11, target.nb.Array[target.nb.Offset + 1]);
            
        }
        
        [Test]
        public void JsonParsing_DateAsString()
        {
            const string json = @"{""value"":""2015-02-26T13:18:13.1521765-08:00""}";

            var target = ParseJson<Box<string>>(json);

            Assert.AreEqual("2015-02-26T13:18:13.1521765-08:00", target.value);
        }

        static T ParseJson<T>(string json) where T : new()
        {
            var deserializer = new Deserializer<SimpleJsonReader>(typeof(T));
            var reader = new SimpleJsonReader(new StringReader(json));
            var target = deserializer.Deserialize<T>(reader);
            return target;
        }

        [Test]
        public void JsonParsing_DateAsStringInStreamConstructor()
        {
            string timestamp = "2017-03-07T04:47:50.1145227Z";
            string json = $"{{\"timestamp\":\"{timestamp}\"}}";

            DateAsString target;
            MemoryStream ms = new MemoryStream(Encoding.UTF8.GetBytes(json));
            target = Deserialize<DateAsString>.From(new SimpleJsonReader(ms));
            Assert.AreEqual(timestamp, target.timestamp);
        }
    }
}
