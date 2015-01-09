// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;
    using System.IO;
    using System.Runtime.CompilerServices;
    using Newtonsoft.Json;

    [Reader(typeof(SimpleJsonReader))]
    public struct SimpleJsonWriter : IProtocolWriter
    {
        public const string NameAttribute = "JsonName";
        readonly JsonTextWriter writer;

        public SimpleJsonWriter(TextWriter writer)
        {
            this.writer = new JsonTextWriter(writer);
        }

        public SimpleJsonWriter(Stream stream)
        {
            writer = new JsonTextWriter(new StreamWriter(stream));
        }

        public void Flush()
        {
            writer.Flush();
        }

        #region IProtocolWriter

        public void WriteVersion()
        {
            throw new NotImplementedException();
        }

        #region Struct

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteStructBegin(Metadata metadata)
        {
            writer.WriteStartObject();
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteStructEnd()
        {
            writer.WriteEndObject();
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBaseBegin(Metadata metadata)
        {}

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBaseEnd()
        {}

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldBegin(BondDataType type, ushort id, Metadata metadata)
        {
            string name;
            writer.WritePropertyName(metadata.attributes.TryGetValue(NameAttribute, out name) ? name : metadata.name);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldEnd()
        {}

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldOmitted(BondDataType type, ushort id, Metadata metadata)
        {}

        #endregion

        #region Containers

        public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
        {
            writer.WriteStartArray();
        }

        public void WriteContainerBegin(int count, BondDataType elementType)
        {
            writer.WriteStartArray();
        }

        public void WriteContainerEnd()
        {
            writer.WriteEndArray();
        }

        #endregion

        #region Scalars

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBool(bool value)
        {
            writer.WriteValue(value);
        }

        public void WriteBytes(ArraySegment<byte> data)
        {
            var end = data.Offset + data.Count;
            for (var i = data.Offset; i != end; ++i)
            {
                writer.WriteValue((sbyte) data.Array[i]);
            }
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteDouble(double value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFloat(float value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt16(short value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt32(int value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt64(long value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteInt8(sbyte value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteString(string value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt16(ushort value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt32(uint value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt64(ulong value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteUInt8(byte value)
        {
            writer.WriteValue(value);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteWString(string value)
        {
            writer.WriteValue(value);
        }

        #endregion

        #endregion
    }
}
