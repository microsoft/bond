namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using Bond;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    public class RuntimeSerializerTests
    {
        [Test]
        public void Serialize_SerializesScalarTypesCorrectlyWhenEverythingIsPopulated()
        {
            var original = new BasicTypes
            {
                _bool = true,
                _str = "String",
                _wstr = "WideString",
                _uint64 = ulong.MaxValue - 6464,
                _uint16 = ushort.MaxValue - 1616,
                _uint32 = uint.MaxValue - 3232,
                _uint8 = byte.MaxValue - 88,
                _int8 = sbyte.MaxValue - 8,
                _int16 = short.MaxValue - 16,
                _int32 = int.MaxValue - 32,
                _int64 = long.MaxValue - 64,
                _double = double.MaxValue / 22,
                _float = float.MaxValue / 2,
                _enum1 = EnumType1.EnumValue4,
                dt = DateTime.UtcNow,
            };

            TestRuntimeSerialization(original);
        }

        [Test]
        public void Serialize_SerializesScalarTypesCorrectlyWhenOnlySomePropertiesArePopulated()
        {
            var original = new BasicTypes
            {
                _str = "String",
                _uint32 = uint.MaxValue - 3232,
                _uint8 = byte.MaxValue - 88,
                _int16 = short.MaxValue - 16,
                _int64 = long.MaxValue - 64,
                _double = double.MaxValue / 22,
                _float = float.MaxValue / 2,
                _enum1 = EnumType1.EnumValue4,
                dt = DateTime.UtcNow,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesDefaultValuesCorrectly()
        {
            var original = new BasicTypes();

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesNestedTypesCorrectly()
        {
            var original = new Nested
            {
                basic = new BasicTypes
                {
                    _str = "Basic",
                },
                nested = new Nested1
                {
                    basic1 = new BasicTypes { _str = "Basic1" },
                    basic2 = new BasicTypes { _str = "Basic2" },
                    guid = new GUID { Data1 = 101, Data2 = 202, Data3 = 303, Data4 = 404 },
                },
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesBondedPropertiesCorrectly()
        {
            var original = new StructWithBonded
            {
                field = new Bonded<Nested>(new Nested
                {
                    basic = new BasicTypes
                    {
                        _str = "Bonded field.basic._str",
                    },
                    nested = new Nested1
                    {
                        guid = new GUID { Data1 = 1, Data2 = 22, Data3 = 333, Data4 = 4444 },
                    },
                }),
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesListPropertiesCorrectly()
        {
            var original = new StructWithByteLists
            {
                b = new List<sbyte> { 1 },
#if BOND_LIST_INTERFACES
                lb = new List<IList<sbyte>> { new List<sbyte> { 21, 22 }, new List<sbyte> { 23, 24 } },
#else
                lb = new List<List<sbyte>> { new List<sbyte> { 21, 22 }, new List<sbyte> { 23, 24 } },
#endif
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesMapPropertiesCorrectly()
        {
            var original = new Maps
            {
                _bool = new Dictionary<string, bool> { { "trueValue", true }, { "falseValue", false } },
                _str = new Dictionary<string, string> { { "stringValue", "Abc123" } },
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesNullablePropertiesCorrectlyWhenNotNull()
        {
            var original = new NullableBasicTypes
            {
                _bool = true,
                _str = "str value",
                _wstr = "wstr value",
                _int8 = -8,
                _int16 = -16,
                _int32 = -32,
                _int64 = -64,
                _uint8 = 8,
                _uint16 = 16,
                _uint32 = 32,
                _uint64 = 64,
                _double = 2468.10,
                _float = 1234.5f,
                _enum1 = EnumType1.EnumValue3,
                dt = DateTime.UtcNow,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesNullablePropertiesCorrectlyWhenNull()
        {
            var original = new NullableBasicTypes
            {
                _bool = null,
                _str = null,
                _wstr = null,
                _int8 = null,
                _int16 = null,
                _int32 = null,
                _int64 = null,
                _uint8 = null,
                _uint16 = null,
                _uint32 = null,
                _uint64 = null,
                _double = null,
                _float = null,
                _enum1 = null,
                dt = null,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesGenericsCorrectly()
        {
            var original = new Generics
            {
                sb = new GenericScalar<bool>
                {
                    field = true,
                    vectorField = new List<bool> { true, false },
                    listGeneric = new LinkedList<GenericScalar<bool>>(
                        new[]
                        {
                            new GenericScalar<bool>
                            {
                                field = true,
                                vectorField = new List<bool> { false, true },
                                nullableField = true,
                                mapField = new Dictionary<bool,bool> { { true, false }, { false, true } },
                            }
                        }
                        ),
                    nullableField = true,
                    mapField = new Dictionary<bool, bool> { { true, true }, { false, false } },
                },
                ci32 = new GenericClass<HashSet<int>>
                {
                    field = new HashSet<int> { 101 },
                    vectorField = new List<HashSet<int>> { new HashSet<int> { 201 } },
                    listGeneric = new LinkedList<GenericClass<HashSet<int>>>(
                        new[]
                        {
                            new GenericClass<HashSet<int>>
                            {
                                field = new HashSet<int> { 301 },
                                vectorField = new List<HashSet<int>> { new HashSet<int> { 302 } },
                                nullableField = new HashSet<int> { 303 },
                                mapField = new Dictionary<string,HashSet<int>>
                                {
                                    { "304", new HashSet<int> { 304 } }
                                },
                            }
                        }),
                    nullableField = new HashSet<int> { 401 },
                    mapField = new Dictionary<string, HashSet<int>> { { "501", new HashSet<int> { 601 } } },
                },
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesFieldsFromBaseTypeCorrectly()
        {
            var original = new DerivedWithMeta
            {
                a = "ValueFromBaseClass",
                b = "ValueFromSubClass",
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesOptionalFieldsCorrectly()
        {
            var original = new WithOptional
            {
                field = 3,
                fieldWithDefault = 4,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesOptionalAliasedFieldsCorrectly()
        {
            var original = new WithOptionalAliased
            {
                field = DateTime.UtcNow,
                fieldWithDefault = DateTime.UtcNow,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesOptionalNullableFieldsCorrectly()
        {
            var original = new WithOptionalNullable
            {
                field = 3,
                fieldWithDefault = 4,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        [Test]
        public void Serialize_SerializesOptionalNullableAliasedFieldsCorrectly()
        {
            var original = new WithOptionalNullableAliased
            {
                field = DateTime.UtcNow,
                fieldWithDefault = DateTime.UtcNow,
            };

            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataSimple(original);
        }

        private static void TestRuntimeSerialization<T>(T original)
        {
            TestRuntimeSerializerDataCompact(original);
            TestRuntimeSerializerDataFast(original);
            TestRuntimeSerializerDataSimple(original);
        }

        private static void TestRuntimeSerializerDataCompact<T>(T obj)
        {
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(obj, writer);

            var runtimeObj = DeserializeCompactWithRuntimeDeserializer<T>(output.Data);

            var runtimeSerializer = new RuntimeSerializer(Schema<T>.RuntimeSchema);

            var runtimeOutput = new OutputBuffer();
            var runtimeWriter = new CompactBinaryWriter<OutputBuffer>(runtimeOutput);

            runtimeSerializer.Serialize(runtimeWriter, runtimeObj);

            VerifyDataMatches(output.Data, runtimeOutput.Data);
        }

        private static void TestRuntimeSerializerDataFast<T>(T obj)
        {
            var serializer = new Serializer<FastBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new FastBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(obj, writer);

            var runtimeObj = DeserializeFastWithRuntimeDeserializer<T>(output.Data);

            var runtimeSerializer = new RuntimeSerializer(Schema<T>.RuntimeSchema);

            var runtimeOutput = new OutputBuffer();
            var runtimeWriter = new FastBinaryWriter<OutputBuffer>(runtimeOutput);

            runtimeSerializer.Serialize(runtimeWriter, runtimeObj);

            VerifyDataMatches(output.Data, runtimeOutput.Data);
        }

        private static void TestRuntimeSerializerDataSimple<T>(T obj)
        {
            var serializer = new Serializer<SimpleBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new SimpleBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(obj, writer);

            var runtimeObj = DeserializeSimpleWithRuntimeDeserializer<T>(output.Data);

            var runtimeSerializer = new RuntimeSerializer(Schema<T>.RuntimeSchema);

            var runtimeOutput = new OutputBuffer();
            var runtimeWriter = new SimpleBinaryWriter<OutputBuffer>(runtimeOutput);

            runtimeSerializer.Serialize(runtimeWriter, runtimeObj);

            VerifyDataMatches(output.Data, runtimeOutput.Data);
        }

        private static RuntimeObject DeserializeCompactWithRuntimeDeserializer<T>(ArraySegment<byte> data)
        {
            var deserializer = new Bond.RuntimeDeserializer(Schema<T>.RuntimeSchema);

            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            return deserializer.Deserialize(reader);
        }

        private static RuntimeObject DeserializeFastWithRuntimeDeserializer<T>(ArraySegment<byte> data)
        {
            var deserializer = new Bond.RuntimeDeserializer(Schema<T>.RuntimeSchema);

            var input = new InputBuffer(data);
            var reader = new FastBinaryReader<InputBuffer>(input);

            return deserializer.Deserialize(reader);
        }

        private static RuntimeObject DeserializeSimpleWithRuntimeDeserializer<T>(ArraySegment<byte> data)
        {
            var deserializer = new Bond.RuntimeDeserializer(Schema<T>.RuntimeSchema);

            var input = new InputBuffer(data);
            var reader = new SimpleBinaryReader<InputBuffer>(input);

            return deserializer.Deserialize(reader);
        }

        private static void VerifyDataMatches(ArraySegment<byte> data, ArraySegment<byte> runtimeData)
        {
            Assert.AreEqual(data.Count, runtimeData.Count);

            for (int i = 0; i < data.Count; i++)
            {
                Assert.AreEqual(data.Array[i], runtimeData.Array[i]);
            }
        }
    }
}
