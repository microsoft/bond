namespace UnitTest
{
    using Bond;
    using Bond.IO.Unsafe;
    using Bond.Protocols;

    using NUnit;
    using NUnit.Framework;

    using System;
    using System.Linq;

    public static class NestedPayloadFactory
    {
        /// <summary>
        /// Creates a payload for a struct like <see cref="ClassWithStructProperties"/> that has an unknown field that is deeply nested.
        /// </summary>
        /// <param name="protocol">The encoding protocol. Simple Binary is not supported.</param>
        /// <param name="protocolVersion">The encoding protocol's version</param>
        /// <param name="depth">How deeply to nest</param>
        /// <param name="fieldType">The type of unknown field to nest. Only <see cref="BondDataType.BT_STRUCT"/>, <see cref="BondDataType.BT_LIST"/>, <see cref="BondDataType.BT_SET"/>, and <see cref="BondDataType.BT_MAP"/> are supported.</param>
        /// <returns>A payload in the <paramref name="protocol"/> encoding protocol.</returns>
        public static ArraySegment<byte> MakeWithUnknownField(ProtocolType protocol, ushort protocolVersion, int depth, BondDataType fieldType)
        {
            if (protocol == ProtocolType.SIMPLE_PROTOCOL)
            {
                throw new NotSupportedException("Simple Binary doesn't support unknown fields.");
            }

            var buffer = new OutputBuffer();
            IProtocolWriter writer = MakeWriter(buffer, protocol, protocolVersion);
            WriteDeeplyNestedPayload(writer, depth, fieldType);
            return buffer.Data;
        }

        /// <summary>
        /// Creates a payload for the struct <see cref="StructWithRecursiveReference"/> that has <paramref name="depth"/> nested child elements.
        /// </summary>
        /// <param name="protocol">The encoding protocol. Simple Binary is not supported.</param>
        /// <param name="protocolVersion">The encoding protocol's version</param>
        /// <param name="depth">How deeply to nest</param>
        /// <returns>A payload in the <paramref name="protocol"/> encoding protocol.</returns>
        public static ArraySegment<byte> MakeWithRecursiveChild(ProtocolType protocol, ushort protocolVersion, int depth)
        {
            var buffer = new OutputBuffer();
            IProtocolWriter writer = MakeWriter(buffer, protocol, protocolVersion);
            WriteRecursiveChild(writer, depth);
            return buffer.Data;
        }

        private static IProtocolWriter MakeWriter(OutputBuffer buffer, ProtocolType protocol, ushort protocolVersion)
        {
            switch (protocol)
            {
                case ProtocolType.COMPACT_PROTOCOL:
                    return new CompactBinaryWriter<OutputBuffer>(buffer, protocolVersion);
                case ProtocolType.FAST_PROTOCOL:
                    return new FastBinaryWriter<OutputBuffer>(buffer, protocolVersion);
                case ProtocolType.SIMPLE_PROTOCOL:
                    return new SimpleBinaryWriter<OutputBuffer>(buffer, protocolVersion);
                default:
                    throw new NotSupportedException("Don't know how to make deeply nested payload for protocol " + protocol);
            }
        }

        private static void WriteDeeplyNestedPayload(IProtocolWriter writer, int depth, BondDataType fieldType)
        {
            ITwoPassProtocolWriter twoPassProtocolWriter = writer as ITwoPassProtocolWriter;
            if (twoPassProtocolWriter != null)
            {
                IProtocolWriter firstPassWriter = twoPassProtocolWriter.GetFirstPassWriter();
                if (firstPassWriter != null)
                {
                    WriteDeeplyNestedPayload(firstPassWriter, depth, fieldType);
                }
            }

            const ushort SomeUnknownFieldId = 50; // shouldn't overlap with any field in Structs.cs

            // use ClassWithStructProperties as a representative struct for testing
            var structMetadata = Schema.GetRuntimeSchema(typeof(ClassWithStructProperties)).StructDef.metadata;
            var fieldMetadata = new Metadata { name = "some_unknown_field", modifier = Modifier.Optional };

            // open top-level field to hold nested values
            writer.WriteStructBegin(structMetadata);
            writer.WriteFieldBegin(fieldType, SomeUnknownFieldId, fieldMetadata);

            // open a level of nesting
            for (int i = 0; i < depth; ++i)
            {
                switch (fieldType)
                {
                    case BondDataType.BT_STRUCT:
                        writer.WriteStructBegin(structMetadata);
                        writer.WriteFieldBegin(fieldType, SomeUnknownFieldId, fieldMetadata);
                        break;
                    case BondDataType.BT_LIST:
                    case BondDataType.BT_SET:
                        writer.WriteContainerBegin(count: 1, elementType: fieldType);
                        break;
                    case BondDataType.BT_MAP:
                        writer.WriteContainerBegin(count: 1, keyType: BondDataType.BT_INT8, valueType: fieldType);
                        writer.WriteInt8(50); // map key
                        break;
                    default:
                        throw new NotSupportedException("Don't know how to create nested payload for field type " + fieldType);
                }
            }

            // write the value inside all the nesting
            switch (fieldType)
            {
                case BondDataType.BT_STRUCT:
                    // empty struct
                    break;
                case BondDataType.BT_LIST:
                case BondDataType.BT_SET:
                    writer.WriteInt8(100); // sequence value
                    break;
                case BondDataType.BT_MAP:
                    writer.WriteInt8(50); // map key
                    writer.WriteInt8(100); // map value
                    break;
                default:
                    throw new NotSupportedException("Don't know how to create nested payload for field type " + fieldType);
            }

            // close a level of nesting
            for (int i = 0; i < depth; ++i)
            {
                switch (fieldType)
                {
                    case BondDataType.BT_STRUCT:
                        writer.WriteFieldEnd();
                        writer.WriteStructEnd();
                        break;
                    case BondDataType.BT_LIST:
                    case BondDataType.BT_SET:
                        writer.WriteContainerEnd();
                        break;
                    case BondDataType.BT_MAP:
                        writer.WriteContainerEnd();
                        break;
                    default:
                        throw new NotSupportedException("Don't know how to create nested payload for field type " + fieldType);
                }
            }

            // close top-level field
            writer.WriteFieldEnd();
            writer.WriteStructEnd();
        }

        private static void WriteRecursiveChild(IProtocolWriter writer, int depth)
        {
            ITwoPassProtocolWriter twoPassProtocolWriter = writer as ITwoPassProtocolWriter;
            if (twoPassProtocolWriter != null)
            {
                IProtocolWriter firstPassWriter = twoPassProtocolWriter.GetFirstPassWriter();
                if (firstPassWriter != null)
                {
                    WriteRecursiveChild(firstPassWriter, depth);
                }
            }

            // get the field id of the recursive child field in the test StructWithRecursiveReference struct
            ushort childFieldId = Reflection
                .GetSchemaFields(typeof(StructWithRecursiveReference))
                .Where(f => f.Name == nameof(StructWithRecursiveReference.Child))
                .Single()
                .Id;

            StructDef structDef = Schema.GetRuntimeSchema(typeof(StructWithRecursiveReference)).StructDef;
            FieldDef ValueFieldDef = structDef.fields.Where(fd => fd.id == 0).Single();
            FieldDef ChildFieldDef = structDef.fields.Where(fd => fd.id == 1).Single();

            // sanity check the struct has the schema we expect
            Assert.AreEqual(BondDataType.BT_UINT32, ValueFieldDef.type.id);
            Assert.AreEqual(BondDataType.BT_LIST, ChildFieldDef.type.id);

            // open top-level struct
            writer.WriteStructBegin(structDef.metadata);

            // open a level of nesting
            for (int i = 0; i < depth; ++i)
            {
                writer.WriteFieldBegin(BondDataType.BT_UINT32, ValueFieldDef.id, ValueFieldDef.metadata);
                writer.WriteUInt32(1000);
                writer.WriteFieldEnd();

                writer.WriteFieldBegin(BondDataType.BT_LIST, childFieldId, ChildFieldDef.metadata);
                writer.WriteContainerBegin(1, BondDataType.BT_STRUCT);
                writer.WriteStructBegin(structDef.metadata);
            }

            // write an empty struct inside all the nesting
            writer.WriteStructBegin(structDef.metadata);
            writer.WriteFieldOmitted(BondDataType.BT_UINT32, ValueFieldDef.id, ValueFieldDef.metadata);
            writer.WriteFieldOmitted(BondDataType.BT_LIST, childFieldId, ChildFieldDef.metadata);
            writer.WriteStructEnd();

            // close a level of nesting
            for (int i = 0; i < depth; ++i)
            {
                writer.WriteStructEnd();
                writer.WriteContainerEnd();
                writer.WriteFieldEnd();
            }

            // close top-level struct
            writer.WriteStructEnd();
        }
    }
}
