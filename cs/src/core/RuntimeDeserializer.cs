// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using Bond.IO.Safe;
    using Bond.Protocols;

    /// <summary>
    /// Deserializes <see cref="RuntimeObject"/>s from a protocol reader using only a runtime schema.
    /// </summary>
    public static class RuntimeDeserialize
    {
        /// <summary>
        /// Deserializes a <see cref="RuntimeObject"/> from a tagged protocol reader using only a runtime schema.
        /// </summary>
        /// <param name="reader">The tagged protocol reader to read the object data from.</param>
        /// <param name="schema">The schema to use when reading the object data.</param>
        /// <returns>A <see cref="RuntimeObject"/> created from <paramref name="reader"/> and <paramref name="schema"/>.</returns>
        public static RuntimeObject From(IClonableTaggedProtocolReader reader, RuntimeSchema schema)
        {
            return new RuntimeDeserializer(schema).Deserialize(reader);
        }

        /// <summary>
        /// Deserializes a <see cref="RuntimeObject"/> from an untagged protocol reader using only a runtime schema.
        /// </summary>
        /// <param name="reader">The untagged protocol reader to read the object data from.</param>
        /// <param name="schema">The schema to use when reading the object data.</param>
        /// <returns>A <see cref="RuntimeObject"/> created from <paramref name="reader"/> and <paramref name="schema"/>.</returns>
        public static RuntimeObject From(IClonableUntaggedProtocolReader reader, RuntimeSchema schema)
        {
            return new RuntimeDeserializer(schema).Deserialize(reader);
        }
    }

    /// <summary>
    /// Deserializes <see cref="RuntimeObject"/>s from a protocol reader using only a <see cref="RuntimeSchema"/>.
    /// </summary>
    public class RuntimeDeserializer
    {
        private readonly RuntimeSchema schema;

        /// <summary>
        /// Initializes a new instance of the <see cref="RuntimeDeserializer"/> class with a given schema.
        /// </summary>
        /// <param name="schema">The schema to use when deserializing objects.</param>
        public RuntimeDeserializer(RuntimeSchema schema)
        {
            if (schema.SchemaDef.root.id != BondDataType.BT_STRUCT)
            {
                throw new ArgumentException("The root of the schema must be a struct (id == BT_STRUCT).",
                    "schema");
            }

            this.schema = schema;
        }

        /// <summary>
        /// Deserializes a <see cref="RuntimeObject"/> from a tagged protocol reader.
        /// </summary>
        /// <param name="reader">The tagged protocol reader to read object data from.</param>
        /// <returns>A <see cref="RuntimeObject"/> created from <paramref name="reader"/>.</returns>
        public RuntimeObject Deserialize(IClonableTaggedProtocolReader reader)
        {
            return ReadStruct(reader, schema.SchemaDef.root) ?? new RuntimeObject();
        }

        /// <summary>
        /// Deserializes a <see cref="RuntimeObject"/> from an untagged protocol reader.
        /// </summary>
        /// <param name="reader">The untagged protocol reader to read object data from.</param>
        /// <returns>A <see cref="RuntimeObject"/> created from <paramref name="reader"/>.</returns>
        public RuntimeObject Deserialize(IClonableUntaggedProtocolReader reader)
        {
            return ReadStruct(reader, schema.SchemaDef.root) ?? new RuntimeObject();
        }

        private RuntimeObject ReadStruct(IClonableTaggedProtocolReader reader, TypeDef typeDef)
        {
            var result = new Lazy<RuntimeObject>(() => new RuntimeObject());

            var structDefs = new Stack<StructDef>();
            while (typeDef != null)
            {
                var structDef = schema.SchemaDef.structs[typeDef.struct_def];
                structDefs.Push(structDef);
                typeDef = structDef.base_def;
            }

            reader.ReadStructBegin();

            while (structDefs.Any())
            {
                var structDef = structDefs.Pop();

                bool isBaseStructDef = structDefs.Any();

                if (isBaseStructDef)
                {
                    reader.ReadBaseBegin();
                }

                while (true)
                {
                    BondDataType actualType;
                    ushort actualId;
                    reader.ReadFieldBegin(out actualType, out actualId);

                    if (actualType == BondDataType.BT_STOP || actualType == BondDataType.BT_STOP_BASE)
                    {
                        reader.ReadFieldEnd();
                        break;
                    }

                    var field = structDef.fields.Single(x => x.id == actualId);
                    object fieldValue = ReadFieldValue(reader, field.type, actualType);
                    if (fieldValue != null
                        || field.metadata.modifier == Modifier.Required
                        || field.metadata.modifier == Modifier.RequiredOptional)
                    {
                        result.Value.Properties[field.metadata.name] = fieldValue;
                    }

                    reader.ReadFieldEnd();
                }

                if (isBaseStructDef)
                {
                    reader.ReadBaseEnd();
                }
            }

            reader.ReadStructEnd();

            return result.IsValueCreated ? result.Value : null;
        }

        private RuntimeObject ReadStruct(IClonableUntaggedProtocolReader reader, TypeDef typeDef)
        {
            var result = new Lazy<RuntimeObject>(() => new RuntimeObject());
            
            var structDefs = new Stack<StructDef>();
            do
            {
                var structDef = schema.SchemaDef.structs[typeDef.struct_def];
                structDefs.Push(structDef);
                typeDef = structDef.base_def;
            } while (typeDef != null);

            do
            {
                var structDef = structDefs.Pop();

                foreach (var field in structDef.fields)
                {
                    if (reader.ReadFieldOmitted())
                    {
                        if (field.type.bonded_type)
                        {
                            result.Value.Properties[field.metadata.name] = Activator.CreateInstance(
                                typeof(Bonded<>).MakeGenericType(GetRuntimeType(field.type, false)),
                                null);
                        }
                        else if (field.metadata.modifier == Modifier.Required
                            || field.metadata.modifier == Modifier.RequiredOptional)
                        {
                            result.Value.Properties[field.metadata.name] = GetDefaultValue(field);
                        }

                        continue;
                    }

                    object fieldValue = ReadFieldValue(reader, field.type);
                    if (fieldValue != null)
                    {
                        result.Value.Properties[field.metadata.name] = fieldValue;
                    }
                }
            } while (structDefs.Any());

            return result.IsValueCreated ? result.Value : null;
        }

        private object GetDefaultValue(FieldDef field)
        {
            if (field.metadata.default_value.nothing)
            {
                return null;
            }

            switch (field.type.id)
            {
                case BondDataType.BT_BOOL:
                    return field.metadata.default_value.uint_value != 0;
                case BondDataType.BT_DOUBLE:
                    return field.metadata.default_value.double_value;
                case BondDataType.BT_FLOAT:
                    return (float)field.metadata.default_value.double_value;
                case BondDataType.BT_INT16:
                    return (short)field.metadata.default_value.int_value;
                case BondDataType.BT_INT32:
                    return (int)field.metadata.default_value.int_value;
                case BondDataType.BT_INT64:
                    return (long)field.metadata.default_value.int_value;
                case BondDataType.BT_INT8:
                    return (sbyte)field.metadata.default_value.int_value;
                case BondDataType.BT_LIST:
                case BondDataType.BT_MAP:
                case BondDataType.BT_SET:
                case BondDataType.BT_STRING:
                case BondDataType.BT_STRUCT:
                case BondDataType.BT_UINT16:
                    return (ushort)field.metadata.default_value.uint_value;
                case BondDataType.BT_UINT32:
                    return (uint)field.metadata.default_value.uint_value;
                case BondDataType.BT_UINT64:
                    return (ulong)field.metadata.default_value.uint_value;
                case BondDataType.BT_UINT8:
                    return (byte)field.metadata.default_value.uint_value;
                case BondDataType.BT_WSTRING:
                    return field.metadata.default_value.wstring_value;
            }

            return null;
        }

        private IRuntimeBonded<RuntimeObject> ReadBondedStruct(IUntaggedProtocolReader reader, TypeDef typeDef)
        {
            int containerSize = (int)reader.ReadUInt32();
            
            var protocol = (ProtocolType)reader.ReadUInt16();
            var version = reader.ReadUInt16();

            // Subtract 2 bytes for protocol and 2 bytes for version
            var bondedData = reader.ReadBytes(containerSize - 4);

            switch (protocol)
            {
                case ProtocolType.COMPACT_PROTOCOL:
                    return new RuntimeBonded<RuntimeObject>(
                        new RuntimeDeserializer(schema).ReadStruct(
                            new CompactBinaryReader<InputBuffer>(new InputBuffer(bondedData), version), typeDef));

                case ProtocolType.FAST_PROTOCOL:
                    return new RuntimeBonded<RuntimeObject>(
                        new RuntimeDeserializer(schema).ReadStruct(
                            new FastBinaryReader<InputBuffer>(new InputBuffer(bondedData), version), typeDef));

                case ProtocolType.SIMPLE_PROTOCOL:
                    return new RuntimeBonded<RuntimeObject>(
                        new RuntimeDeserializer(schema).ReadStruct(
                            new SimpleBinaryReader<InputBuffer>(new InputBuffer(bondedData), version), typeDef));

                default:
                    throw new InvalidDataException(string.Format("Unknown ProtocolType {0}", protocol));
            }
        }

        private object ReadContainer(IClonableTaggedProtocolReader reader, TypeDef elementType)
        {
            int count;
            BondDataType actualType;
            reader.ReadContainerBegin(out count, out actualType);

            if (count <= 0)
            {
                return null;
            }

            var itemType = GetRuntimeType(elementType);
            var listType = typeof(List<>).MakeGenericType(itemType);
            var addMethodType = typeof(ICollection<>).MakeGenericType(itemType);
            
            var list = new Lazy<object>(() => Activator.CreateInstance(listType));
            var addMethod = addMethodType.FindMethod("Add", itemType);

            for (int i = 0; i < count; i++)
            {
                var value = ReadFieldValue(reader, elementType, actualType);
                if (value != null)
                {
                    addMethod.Invoke(list.Value, new[] { value });
                }
            }

            reader.ReadContainerEnd();

            return list.IsValueCreated ? list.Value : null;
        }

        private object ReadContainer(IClonableUntaggedProtocolReader reader, TypeDef elementType)
        {
            int count = reader.ReadContainerBegin();
            if (count <= 0)
            {
                return null;
            }

            var itemType = GetRuntimeType(elementType);
            var listType = typeof(List<>).MakeGenericType(itemType);
            var addMethodType = typeof(ICollection<>).MakeGenericType(itemType);

            var list = new Lazy<object>(() => Activator.CreateInstance(listType));
            var addMethod = addMethodType.FindMethod("Add", itemType);

            for (int i = 0; i < count; i++)
            {
                var value = ReadFieldValue(reader, elementType);
                if (value != null)
                {
                    addMethod.Invoke(list.Value, new[] { value });
                }
            }

            reader.ReadContainerEnd();

            return list.IsValueCreated ? list.Value : null;
        }

        private object ReadMap(IClonableTaggedProtocolReader reader, TypeDef keyType, TypeDef valueType)
        {
            var keyRuntimeType = GetRuntimeType(keyType);
            var valueRuntimeType = GetRuntimeType(valueType);
            var dictionaryType = typeof(Dictionary<,>).MakeGenericType(keyRuntimeType, valueRuntimeType);
            var addMethodType = typeof(IDictionary<,>).MakeGenericType(keyRuntimeType, valueRuntimeType);

            var dictionary = new Lazy<object>(() => Activator.CreateInstance(dictionaryType));
            var addMethod = addMethodType.FindMethod("Add", keyRuntimeType, valueRuntimeType);

            int count;
            BondDataType actualKeyType;
            BondDataType actualValueType;
            reader.ReadContainerBegin(out count, out actualKeyType, out actualValueType);

            for (int i = 0; i < count; i++)
            {
                var key = ReadFieldValue(reader, keyType, actualKeyType);
                var value = ReadFieldValue(reader, valueType, actualValueType);

                addMethod.Invoke(dictionary.Value, new[] { key, value });
            }

            return dictionary.IsValueCreated ? dictionary.Value : null;
        }

        private object ReadMap(IClonableUntaggedProtocolReader reader, TypeDef keyType, TypeDef valueType)
        {
            var keyRuntimeType = GetRuntimeType(keyType);
            var valueRuntimeType = GetRuntimeType(valueType);
            var dictionaryType = typeof(Dictionary<,>).MakeGenericType(keyRuntimeType, valueRuntimeType);
            var addMethodType = typeof(IDictionary<,>).MakeGenericType(keyRuntimeType, valueRuntimeType);

            var dictionary = new Lazy<object>(() => Activator.CreateInstance(dictionaryType));
            var addMethod = addMethodType.FindMethod("Add", keyRuntimeType, valueRuntimeType);

            int count = reader.ReadContainerBegin();
            
            for (int i = 0; i < count; i++)
            {
                var key = ReadFieldValue(reader, keyType);
                var value = ReadFieldValue(reader, valueType);

                addMethod.Invoke(dictionary.Value, new[] { key, value });
            }

            return dictionary.IsValueCreated ? dictionary.Value : null;
        }

        private object ReadFieldValue(IClonableTaggedProtocolReader reader, TypeDef fieldType,
            BondDataType actualType)
        {
            switch (actualType)
            {
                case BondDataType.BT_BOOL:
                    {
                        return reader.ReadBool();
                    }
                case BondDataType.BT_INT8:
                    {
                        return reader.ReadInt8();
                    }
                case BondDataType.BT_UINT8:
                    {
                        return reader.ReadUInt8();
                    }
                case BondDataType.BT_INT16:
                    {
                        return reader.ReadInt16();
                    }
                case BondDataType.BT_UINT16:
                    {
                        return reader.ReadUInt16();
                    }
                case BondDataType.BT_INT32:
                    {
                        return reader.ReadInt32();
                    }
                case BondDataType.BT_UINT32:
                    {
                        return reader.ReadUInt32();
                    }
                case BondDataType.BT_INT64:
                    {
                        return reader.ReadInt64();
                    }
                case BondDataType.BT_UINT64:
                    {
                        return reader.ReadUInt64();
                    }
                case BondDataType.BT_FLOAT:
                    {
                        return reader.ReadFloat();
                    }
                case BondDataType.BT_DOUBLE:
                    {
                        return reader.ReadDouble();
                    }
                case BondDataType.BT_STRING:
                    {
                        return reader.ReadString();
                    }
                case BondDataType.BT_WSTRING:
                    {
                        return reader.ReadWString();
                    }
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                    {
                        if (fieldType.bonded_type)
                        {
                            var readerClone = reader.Clone();

                            int itemCount;
                            BondDataType itemType;
                            reader.ReadContainerBegin(out itemCount, out itemType);
                            for (int i = 0; i < itemCount; i++)
                            {
                                reader.Skip(itemType);
                            }
                            reader.ReadContainerEnd();

                            return CreateRuntimeBonded(() => ReadContainer(readerClone, fieldType.element));
                        }

                        return ReadContainer(reader, fieldType.element);
                    }
                case BondDataType.BT_MAP:
                    {
                        if (fieldType.bonded_type)
                        {
                            var readerClone = reader.Clone();

                            int itemCount;
                            BondDataType actualKeyType;
                            BondDataType actualValueType;
                            reader.ReadContainerBegin(out itemCount, out actualKeyType, out actualValueType);
                            for (int i = 0; i < itemCount; i++)
                            {
                                reader.Skip(actualKeyType);
                                reader.Skip(actualValueType);
                            }
                            reader.ReadContainerEnd();

                            return CreateRuntimeBonded(() => ReadMap(readerClone, fieldType.key, fieldType.element));
                        }

                        return ReadMap(reader, fieldType.key, fieldType.element);
                    }
                case BondDataType.BT_STRUCT:
                    {
                        if (fieldType.bonded_type)
                        {
                            var readerClone = reader.Clone();

                            reader.Skip(actualType);

                            return CreateRuntimeBonded(() => ReadStruct(readerClone, fieldType));
                        }

                        return ReadStruct(reader, fieldType);
                    }
                case BondDataType.BT_UNAVAILABLE:
                default:
                    return fieldType.bonded_type ? new RuntimeBonded<object>(null) : null;
            }
        }

        private object ReadFieldValue(IClonableUntaggedProtocolReader reader, TypeDef fieldType)
        {
            switch (fieldType.id)
            {
                case BondDataType.BT_BOOL:
                    return reader.ReadBool();
                case BondDataType.BT_INT8:
                    return reader.ReadInt8();
                case BondDataType.BT_UINT8:
                    return reader.ReadUInt8();
                case BondDataType.BT_INT16:
                    return reader.ReadInt16();
                case BondDataType.BT_UINT16:
                    return reader.ReadUInt16();
                case BondDataType.BT_INT32:
                    return reader.ReadInt32();
                case BondDataType.BT_UINT32:
                    return reader.ReadUInt32();
                case BondDataType.BT_INT64:
                    return reader.ReadInt64();
                case BondDataType.BT_UINT64:
                    return reader.ReadUInt64();
                case BondDataType.BT_FLOAT:
                    return reader.ReadFloat();
                case BondDataType.BT_DOUBLE:
                    return reader.ReadDouble();
                case BondDataType.BT_STRING:
                    return reader.ReadString();
                case BondDataType.BT_WSTRING:
                    return reader.ReadWString();
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                    {
                        if (fieldType.bonded_type)
                        {
                            var readerClone = reader.Clone();
                            
                            int itemCount = reader.ReadContainerBegin();
                            for (int i = 0; i < itemCount; i++)
                            {
                                switch (fieldType.element.id)
                                {
                                    case BondDataType.BT_STRING:
                                        reader.SkipString();
                                        break;
                                    default:
                                        throw new NotImplementedException();
                                }
                            }
                            reader.ReadContainerEnd();

                            return CreateRuntimeBonded(() => ReadContainer(readerClone, fieldType.element));
                        }

                        return ReadContainer(reader, fieldType.element);
                    }
                case BondDataType.BT_MAP:
                    {
                        if (fieldType.bonded_type)
                        {
                            var readerClone = reader.Clone();

                            int itemCount = reader.ReadContainerBegin();
                            for (int i = 0; i < itemCount; i++)
                            {
                                switch (fieldType.key.id)
                                {
                                    case BondDataType.BT_STRING:
                                        reader.SkipString();
                                        break;
                                    default:
                                        throw new NotImplementedException();
                                }

                                switch (fieldType.element.id)
                                {
                                    case BondDataType.BT_STRING:
                                        reader.SkipString();
                                        break;
                                    default:
                                        throw new NotImplementedException();
                                }
                            }

                            return CreateRuntimeBonded(() => ReadMap(readerClone, fieldType.key, fieldType.element));
                        }

                        return ReadMap(reader, fieldType.key, fieldType.element);
                    }
                case BondDataType.BT_STRUCT:
                    {
                        if (fieldType.bonded_type)
                        {
                            var readerClone = reader.Clone();

                            int structSize = (int)reader.ReadUInt32();
                            reader.SkipBytes(structSize);
                            
                            return ReadBondedStruct(readerClone, fieldType);
                        }

                        return ReadStruct(reader, fieldType);
                    }
                case BondDataType.BT_UNAVAILABLE:
                default:
                    return fieldType.bonded_type ? new RuntimeBonded<object>(null) : null;
            }
        }

        private static Type GetRuntimeType(TypeDef elementType, bool shouldWrapBonded = false)
        {
            Type runtimeType;

            switch (elementType.id)
            {
                case BondDataType.BT_BOOL:
                    runtimeType = typeof(bool);
                    break;
                case BondDataType.BT_INT8:
                    runtimeType = typeof(sbyte);
                    break;
                case BondDataType.BT_UINT8:
                    runtimeType = typeof(byte);
                    break;
                case BondDataType.BT_INT16:
                    runtimeType = typeof(short);
                    break;
                case BondDataType.BT_UINT16:
                    runtimeType = typeof(ushort);
                    break;
                case BondDataType.BT_INT32:
                    runtimeType = typeof(int);
                    break;
                case BondDataType.BT_UINT32:
                    runtimeType = typeof(uint);
                    break;
                case BondDataType.BT_INT64:
                    runtimeType = typeof(long);
                    break;
                case BondDataType.BT_UINT64:
                    runtimeType = typeof(ulong);
                    break;
                case BondDataType.BT_FLOAT:
                    runtimeType = typeof(float);
                    break;
                case BondDataType.BT_DOUBLE:
                    runtimeType = typeof(double);
                    break;
                case BondDataType.BT_STRING:
                case BondDataType.BT_WSTRING:
                    runtimeType = typeof(string);
                    break;
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                    runtimeType = typeof(List<>).MakeGenericType(GetRuntimeType(elementType.element));
                    break;
                case BondDataType.BT_MAP:
                    runtimeType = typeof(Dictionary<,>).MakeGenericType(
                        GetRuntimeType(elementType.key), GetRuntimeType(elementType.element));
                    break;
                case BondDataType.BT_STRUCT:
                    runtimeType = typeof(RuntimeObject);
                    break;
                case BondDataType.BT_UNAVAILABLE:
                default:
                    throw new ArgumentException("No type is available for the given bond data type.");
            }

            return elementType.bonded_type && shouldWrapBonded
                ? typeof(IRuntimeBonded<>).MakeGenericType(runtimeType)
                : runtimeType;
        }
        
        private static object CreateRuntimeBonded<T>(Func<T> valueDeserializer)
        {
            return Activator.CreateInstance(typeof(RuntimeBonded<T>), valueDeserializer);
        }
    }
}
