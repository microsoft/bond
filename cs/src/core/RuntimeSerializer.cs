// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;
    using Bond.IO;
    using Bond.IO.Safe;
    using Bond.Protocols;

    /// <summary>
    /// Serializes <see cref="RuntimeObject"/>s to a protocol writer using only a runtime schema.
    /// </summary>
    public static class RuntimeSerialize
    {
        /// <summary>
        /// Serializes a <see cref="RuntimeObject"/> to a protocol writer using only a runtime schema.
        /// </summary>
        /// <param name="writer">The protocol writer to write the object data to.</param>
        /// <param name="schema">The schema to use when writing the object data.</param>
        /// <param name="obj">The object to serialize.</param>
        public static void To(IProtocolWriter writer, RuntimeSchema schema, RuntimeObject obj)
        {
            new RuntimeSerializer(schema).Serialize(writer, obj);
        }
    }

    /// <summary>
    /// Serializes <see cref="RuntimeObject"/>s to a protocol writer using only a runtime schema.
    /// </summary>
    public class RuntimeSerializer
    {
        private readonly RuntimeSchema schema;

        /// <summary>
        /// Initializes a new instance of the <see cref="RuntimeSerializer"/> class with a given runtime schema.
        /// </summary>
        /// <param name="schema">The schema to use when serializing objects.</param>
        public RuntimeSerializer(RuntimeSchema schema)
        {
            this.schema = schema;
        }

        /// <summary>
        /// Serializes a <see cref="RuntimeObject"/> to a protocol writer.
        /// </summary>
        /// <param name="writer">The protocol writer to write object data to.</param>
        /// <param name="obj">The object to serialize.</param>
        public void Serialize(IProtocolWriter writer, RuntimeObject obj)
        {
            if (obj == null)
            {
                throw new ArgumentNullException("obj");
            }

            WriteStruct(writer, obj, schema.SchemaDef.root);
        }

        private void WriteStruct(IProtocolWriter writer, RuntimeObject obj, TypeDef structType)
        {
            var topLevelStructDef = schema.SchemaDef.structs[structType.struct_def];

            var structDefs = new Stack<StructDef>();
            while (structType != null)
            {
                var structDef = schema.SchemaDef.structs[structType.struct_def];
                structDefs.Push(structDef);
                structType = structDef.base_def;
            }

            writer.WriteStructBegin(topLevelStructDef.metadata);

            while (structDefs.Any())
            {
                var structDef = structDefs.Pop();

                bool isBaseStructDef = structDefs.Any();

                if (isBaseStructDef)
                {
                    writer.WriteBaseBegin(structDef.metadata);
                }

                foreach (var field in structDef.fields)
                {
                    WriteField(writer, obj, field);
                }

                if (isBaseStructDef)
                {
                    writer.WriteBaseEnd();
                }
            }

            writer.WriteStructEnd();
        }

        private void WriteField(IProtocolWriter writer, RuntimeObject obj, FieldDef field)
        {
            bool fieldExists = obj != null && obj.Properties.ContainsKey(field.metadata.name);

            if (!fieldExists && field.type.id != BondDataType.BT_STRUCT)
            {
                writer.WriteFieldOmitted(field.type.id, field.id, field.metadata);
                return;
            }

            object value = fieldExists ? obj.Properties[field.metadata.name] : null;

            if (field.type.bonded_type)
            {
                if (value is IRuntimeBonded)
                {
                    value = ((IRuntimeBonded)value).Deserialize();

                    writer.WriteFieldBegin(field.type.id, field.id, field.metadata);

                    // All bonded fields appear to always be serialized using the CompactBinary format
                    if (writer.GetType().GetGenericTypeDefinition() == typeof(CompactBinaryWriter<>))
                    {
                        // If the writer is already using CompactBinary, just write the bonded data
                        WriteValue(writer, value, field.type);
                    }
                    else
                    {
                        // If the writer is not already using CompactBinary, serialize the bonded field value using
                        // CompactBinary then write it as a byte array.
                        var bondedBuffer = new OutputBuffer();
                        var bondedWriter = new CompactBinaryWriter<OutputBuffer>(bondedBuffer);

                        bondedWriter.WriteVersion();
                        WriteValue(bondedWriter, value, field.type);

                        writer.WriteUInt32((uint)bondedBuffer.Data.Count);
                        writer.WriteBytes(bondedBuffer.Data);
                    }

                    writer.WriteFieldEnd();
                }
            }
            else
            {
                if (value == null && field.type.id != BondDataType.BT_STRUCT)
                {
                    writer.WriteFieldOmitted(field.type.id, field.id, field.metadata);
                }
                else
                {
                    writer.WriteFieldBegin(field.type.id, field.id, field.metadata);

                    WriteValue(writer, value, field.type);

                    writer.WriteFieldEnd();
                }
            }
        }

        private void WriteValue(IProtocolWriter writer, object obj, TypeDef fieldType)
        {
            switch (fieldType.id)
            {
                case BondDataType.BT_BOOL:
                    writer.WriteBool((bool)obj);
                    break;
                case BondDataType.BT_INT8:
                    writer.WriteInt8((sbyte)obj);
                    break;
                case BondDataType.BT_UINT8:
                    writer.WriteUInt8((byte)obj);
                    break;
                case BondDataType.BT_INT16:
                    writer.WriteInt16((short)obj);
                    break;
                case BondDataType.BT_UINT16:
                    writer.WriteUInt16((ushort)obj);
                    break;
                case BondDataType.BT_INT32:
                    writer.WriteInt32((int)obj);
                    break;
                case BondDataType.BT_UINT32:
                    writer.WriteUInt32((uint)obj);
                    break;
                case BondDataType.BT_INT64:
                    writer.WriteInt64((long)obj);
                    break;
                case BondDataType.BT_UINT64:
                    writer.WriteUInt64((ulong)obj);
                    break;
                case BondDataType.BT_FLOAT:
                    writer.WriteFloat((float)obj);
                    break;
                case BondDataType.BT_DOUBLE:
                    writer.WriteDouble((double)obj);
                    break;
                case BondDataType.BT_STRING:
                    writer.WriteString((string)obj);
                    break;
                case BondDataType.BT_WSTRING:
                    writer.WriteWString((string)obj);
                    break;
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                    WriteContainer(writer, obj, fieldType.element);
                    break;
                case BondDataType.BT_MAP:
                    WriteMap(writer, obj, fieldType.key, fieldType.element);
                    break;
                case BondDataType.BT_STRUCT:
                    WriteStruct(writer, (RuntimeObject)obj, fieldType);
                    break;
            }
        }

        private void WriteContainer(IProtocolWriter writer, object value, TypeDef elementType)
        {
            var list = (ICollection)value;

            writer.WriteContainerBegin(list.Count, elementType.id);

            foreach (object item in list)
            {
                WriteValue(writer, item, elementType);
            }

            writer.WriteContainerEnd();
        }

        private void WriteMap(IProtocolWriter writer, object value, TypeDef keyType, TypeDef valueType)
        {
            var dictionary = (IDictionary)value;

            writer.WriteContainerBegin(dictionary.Count, keyType.id, valueType.id);

            foreach (object key in dictionary.Keys)
            {
                WriteValue(writer, key, keyType);
                WriteValue(writer, dictionary[key], valueType);
            }

            writer.WriteContainerEnd();
        }
    }
}
