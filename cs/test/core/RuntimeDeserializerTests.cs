namespace UnitTest
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;
    using Bond;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    public class RuntimeDeserializerTests
    {
        [Test]
        public void Deserialize_DeserializesScalarTypesCorrectlyWhenEverythingIsPopulated()
        {
            var expected = new BasicTypes
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

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesScalarTypesCorrectlyWhenOnlySomePropertiesArePopulated()
        {
            var expected = new BasicTypes
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

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesNestedTypes()
        {
            var expected = new Nested
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

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesBondedProperties()
        {
            var expected = new StructWithBonded
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

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesListProperties()
        {
            var expected = new StructWithByteLists
            {
                b = new List<sbyte> { 1 },
#if BOND_LIST_INTERFACES
                lb = new List<IList<sbyte>> { new List<sbyte> { 21, 22 }, new List<sbyte> { 23, 24 } },
#else
                lb = new List<List<sbyte>> { new List<sbyte> { 21, 22 }, new List<sbyte> { 23, 24 } },
#endif
                nb = new List<sbyte>(),
            };

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesMapProperties()
        {
            var expected = new Maps
            {
                _bool = new Dictionary<string, bool> { { "trueValue", true }, {"falseValue", false } },
                _str = new Dictionary<string, string> { { "stringValue", "Abc123" } },
            };

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesNullablePropertiesWhenNotNull()
        {
            var expected = new NullableBasicTypes
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

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesNullablePropertiesWhenNull()
        {
            var expected = new NullableBasicTypes
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

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesGenerics()
        {
            var expected = new Generics
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
                cbt = new GenericClass<BasicTypes>
                {
                    field = new BasicTypes
                    {
                        _enum1 = EnumType1.EnumValue1,
                    },
                },
            };

            TestRuntimeDeserialization(expected);
        }

        [Test]
        public void Deserialize_DeserializesFieldsFromBaseType()
        {
            var expected = new DerivedWithMeta
            {
                a = "ValueFromBaseClass",
                b = "ValueFromSubClass",
            };

            TestRuntimeDeserialization(expected);
        }

        private static void TestRuntimeDeserialization<T>(T expected)
        {
            TestWithRuntimeDeserializerCompact(expected);
            TestWithRuntimeDeserializerSimple(expected);
        }

        private static ArraySegment<byte> SerializeCompact<T>(T original)
        {
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(original, writer);

            return output.Data;
        }

        private static ArraySegment<byte> SerializeFast<T>(T original)
        {
            var serializer = new Serializer<FastBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new FastBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(original, writer);

            return output.Data;
        }

        private static ArraySegment<byte> SerializeSimple<T>(T original)
        {
            var serializer = new Serializer<SimpleBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new SimpleBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(original, writer);

            return output.Data;
        }

        private static void TestWithRuntimeDeserializerCompact<T>(T original)
        {
            var serializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(original, writer);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            var expected = Deserialize<T>.From(reader);

            var runtimeDeserializer = new RuntimeDeserializer(Schema<T>.RuntimeSchema);

            var runtimeInput = new InputBuffer(output.Data);
            var runtimeReader = new CompactBinaryReader<InputBuffer>(runtimeInput);

            var actual = runtimeDeserializer.Deserialize(runtimeReader);

            VerifyStructsMatch(expected, actual);
        }

        private static void TestWithRuntimeDeserializerFast<T>(T original)
        {
            var serializer = new Serializer<FastBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new FastBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(original, writer);

            var input = new InputBuffer(output.Data);
            var reader = new FastBinaryReader<InputBuffer>(input);
            var expected = Deserialize<T>.From(reader);

            var runtimeDeserializer = new RuntimeDeserializer(Schema<T>.RuntimeSchema);

            var runtimeInput = new InputBuffer(output.Data);
            var runtimeReader = new FastBinaryReader<InputBuffer>(runtimeInput);

            var actual = runtimeDeserializer.Deserialize(runtimeReader);

            VerifyStructsMatch(expected, actual);
        }

        private static void TestWithRuntimeDeserializerSimple<T>(T original)
        {
            var serializer = new Serializer<SimpleBinaryWriter<OutputBuffer>>(typeof(T));

            var output = new OutputBuffer();
            var writer = new SimpleBinaryWriter<OutputBuffer>(output);

            serializer.Serialize(original, writer);

            var input = new InputBuffer(output.Data);
            var reader = new SimpleBinaryReader<InputBuffer>(input);
            var expected = Deserialize<T>.From(reader);

            var runtimeDeserializer = new RuntimeDeserializer(Schema<T>.RuntimeSchema);

            var runtimeInput = new InputBuffer(output.Data);
            var runtimeReader = new SimpleBinaryReader<InputBuffer>(runtimeInput);

            var actual = runtimeDeserializer.Deserialize(runtimeReader);

            VerifyStructsMatch(expected, actual);
        }

        private static void VerifyStructsMatch(object expected, RuntimeObject actual)
        {
            var propertiesVerified = new List<string>();

            foreach (string propertyName in actual.Properties.Keys)
            {
                var expectedValue = expected.GetType().GetProperty(propertyName).GetValue(expected, null);
                var actualValue = actual.Properties[propertyName];

                VerifyObjectsMatch(propertyName, expectedValue, actualValue);

                propertiesVerified.Add(propertyName);
            }
        }

        private static void VerifyObjectsMatch(string propertyName, object expectedValue, object actualValue)
        {
            if (expectedValue == null && actualValue == null)
            {
                return;
            }

            if (expectedValue == null || actualValue == null)
            {
                Assert.Fail(string.Format(
                    "Expected and actual values of the {0} property do not match. One of them is null.",
                    propertyName));
            }

            if (ImplementsInterface(actualValue, typeof(IRuntimeBonded<>)))
            {
                if (!ImplementsInterface(expectedValue, typeof(IBonded<>)))
                {
                    Assert.Fail("The actual value is bonded, but the expected value is not.");
                }

                expectedValue = ((IBonded<object>)expectedValue).Deserialize();
                actualValue = ((IRuntimeBonded)actualValue).Deserialize();
            }

            if (ImplementsDictionaryInterface(actualValue))
            {
                if (!ImplementsDictionaryInterface(expectedValue))
                {
                    Assert.Fail("The actual value is a dictionary, but the expected value is not.");
                }

                var expectedDictionary = GetAsDictionary(expectedValue);
                var actualDictionary = GetAsDictionary(actualValue);

                Assert.AreEqual(expectedDictionary.Count, actualDictionary.Count);
                foreach (object key in expectedDictionary.Keys)
                {
                    VerifyObjectsMatch(string.Format("{0}[{1}]", propertyName, key), expectedDictionary[key],
                        actualDictionary[key]);
                }
            }
            else if (ImplementsCollectionInterface(actualValue))
            {
                var actualList = GetAsList(actualValue);

                if (!ImplementsCollectionInterface(expectedValue))
                {
                    switch (actualList.Count)
                    {
                        case 0:
                            VerifyObjectsMatch(propertyName, expectedValue, null);
                            return;
                        case 1:
                            VerifyObjectsMatch(propertyName, expectedValue, actualList[0]);
                            return;
                        default:
                            Assert.Fail("The actual value is a list, but the expected value is not.");
                            break;
                    }
                }

                var expectedList = GetAsList(expectedValue);

                Assert.AreEqual(expectedList.Count, actualList.Count);

                for (int i = 0; i < expectedList.Count; i++)
                {
                    VerifyObjectsMatch(string.Format("{0}[{1}]", propertyName, i), expectedList[i], actualList[i]);
                }
            }
            else if (actualValue is RuntimeObject)
            {
                VerifyStructsMatch(expectedValue, (RuntimeObject)actualValue);
            }
            else
            {
                VerifyScalarValuesMatch(expectedValue, actualValue);
            }
        }

        private static bool ImplementsInterface(object actualValue, Type genericTypeDefinition)
        {
            if (genericTypeDefinition.IsGenericType)
            {
                genericTypeDefinition = genericTypeDefinition.GetGenericTypeDefinition();
            }

            return actualValue.GetType()
                .GetInterfaces()
                .Select(x => x.IsGenericType ? x.GetGenericTypeDefinition() : x)
                .Any(x => x == genericTypeDefinition);
        }

        private static bool ImplementsCollectionInterface(object actualValue)
        {
            return ImplementsInterface(actualValue, typeof(ICollection))
                || ImplementsInterface(actualValue, typeof(ICollection<>));
        }

        private static bool ImplementsDictionaryInterface(object actualValue)
        {
            return ImplementsInterface(actualValue, typeof(IDictionary))
                || ImplementsInterface(actualValue, typeof(IDictionary<,>));
        }

        private static IList GetAsList(object actualValue)
        {
            if (ImplementsInterface(actualValue, typeof(ICollection)))
            {
                return new ArrayList((ICollection)actualValue);
            }

            if (ImplementsInterface(actualValue, typeof(IEnumerable)))
            {
                var list = new ArrayList();

                foreach (object obj in (IEnumerable)actualValue)
                {
                    list.Add(obj);
                }

                return list;
            }

            throw new ArgumentException("The object does not implement a known collection interface.");
        }

        private static IDictionary GetAsDictionary(object actualValue)
        {
            if (actualValue is IDictionary)
            {
                return (IDictionary)actualValue;
            }

            if (ImplementsInterface(actualValue, typeof(IEnumerable)))
            {
                var hashtable = new Hashtable();

                var keyProperty = typeof(KeyValuePair<,>).GetProperty("Key");
                var valueProperty = typeof(KeyValuePair<,>).GetProperty("Value");

                foreach (var kvp in (IEnumerable)actualValue)
                {
                    hashtable.Add(keyProperty.GetValue(kvp, null), valueProperty.GetValue(kvp, null));
                }

                return hashtable;
            }

            throw new ArgumentException("The object does not implement a known dictionary interface.");
        }

        private static void VerifyScalarValuesMatch(object expectedValue, object actualValue)
        {
            if (actualValue.GetType() == typeof(bool))
            {
                Assert.AreEqual((bool)expectedValue, (bool)actualValue);
            }
            else if (actualValue.GetType() == typeof(sbyte))
            {
                Assert.AreEqual((sbyte)expectedValue, (sbyte)actualValue);
            }
            else if (actualValue.GetType() == typeof(short))
            {
                Assert.AreEqual((short)expectedValue, (short)actualValue);
            }
            else if (actualValue.GetType() == typeof(int)
                || expectedValue.GetType().IsEnum)
            {
                Assert.AreEqual((int)expectedValue, (int)actualValue);
            }
            else if (actualValue.GetType() == typeof(long))
            {
                if (expectedValue.GetType() == typeof(DateTime))
                {
                    Assert.AreEqual(((DateTime)expectedValue).Ticks, (long)actualValue);
                }
                else
                {
                    Assert.AreEqual((long)expectedValue, (long)actualValue);
                }
            }
            else if (actualValue.GetType() == typeof(byte))
            {
                Assert.AreEqual((byte)expectedValue, (byte)actualValue);
            }
            else if (actualValue.GetType() == typeof(ushort))
            {
                Assert.AreEqual((ushort)expectedValue, (ushort)actualValue);
            }
            else if (actualValue.GetType() == typeof(uint))
            {
                Assert.AreEqual((uint)expectedValue, (uint)actualValue);
            }
            else if (actualValue.GetType() == typeof(ulong))
            {
                Assert.AreEqual((ulong)expectedValue, (ulong)actualValue);
            }
            else if (actualValue.GetType() == typeof(float))
            {
                Assert.AreEqual((float)expectedValue, (float)actualValue);
            }
            else if (actualValue.GetType() == typeof(double))
            {
                Assert.AreEqual((double)expectedValue, (double)actualValue);
            }
            else if (actualValue.GetType() == typeof(string))
            {
                Assert.AreEqual((string)expectedValue, (string)actualValue);
            }
            else
            {
                Assert.AreEqual(expectedValue.ToString(), actualValue.ToString());
            }
        }
    }
}
