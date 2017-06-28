// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Runtime.CompilerServices;
    using System.Xml;

    [Reader(typeof(SimpleXmlReader))]
    public struct SimpleXmlWriter : IProtocolWriter, ITextProtocolWriter
    {
        public struct Settings
        {
            public static readonly Settings Default = new Settings();
            public bool UseNamespaces { set; get; }
        }
        
        readonly XmlWriter writer;
        readonly Stack<string> ns;

        public SimpleXmlWriter(XmlWriter writer)
            : this(writer, Settings.Default)
        {}

        public SimpleXmlWriter(Stream stream)
            : this(stream, Settings.Default)
        {}

        public SimpleXmlWriter(Stream stream, Settings settings)
            : this(XmlWriter.Create(stream, new XmlWriterSettings { OmitXmlDeclaration = true, Indent = true }), settings)
        {}

        public SimpleXmlWriter(XmlWriter writer, Settings settings)
        {
            this.writer = writer;
            ns = settings.UseNamespaces ? new Stack<string>() : null;
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

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteStructBegin(Metadata metadata)
        {
            writer.WriteStartElement(metadata.GetXmlName());
            PushNamespace(metadata);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBaseBegin(Metadata metadata)
        {
            PushNamespace(metadata);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteStructEnd()
        {
            PopNamespace();
            writer.WriteEndElement();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBaseEnd()
        {
            PopNamespace();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldBegin(BondDataType dataType, ushort id, Metadata metadata)
        {
            writer.WriteStartElement(Prefix, metadata.name, null);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldEnd()
        {
            writer.WriteEndElement();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFieldOmitted(BondDataType dataType, ushort id, Metadata metadata)
        { }

        #endregion

        #region Containers

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerBegin(int count, BondDataType elementType)
        { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
        { }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteContainerEnd()
        { }

        #region ITextProtocolWriter

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteItemBegin()
        {
            writer.WriteStartElement("Item");
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteItemEnd()
        {
            writer.WriteEndElement();
        }

        #endregion

        #endregion

        #region Scalars

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt8(sbyte value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt16(short value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt32(int value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteInt64(long value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt8(byte value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt16(ushort value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt32(uint value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteUInt64(ulong value)
        {
            writer.WriteValue(value.ToString());
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteFloat(float value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteDouble(double value)
        {
            writer.WriteValue(value);
        }

        public void WriteBytes(ArraySegment<byte> data)
        {
            // TODO: for now handle blob as array of bytes; consider CDATA
            var end = data.Offset + data.Count;
            for (var i = data.Offset; i != end; ++i)
            {
                WriteItemBegin();
                WriteInt8((sbyte)data.Array[i]);
                WriteItemEnd();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteBool(bool value)
        {
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteString(string value)
        {
            // Other protocols depend on expressions such as value.Count to
            // throw an NRE if we've been asked to serialize a non-nullable
            // string field that is set to null. Implementations of
            // System.Xml.XmlWriter may successfully serialize it, so we need
            // to check and throw explicitly before that.
            if (value == null)
            {
                throw new NullReferenceException(
                   "Attempted to serialize a null string. This may indicate a non-nullable string field that was set to null.");
            }
            writer.WriteValue(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteWString(string value)
        {
            // Other protocols depend on expressions such as value.Count to
            // throw an NRE if we've been asked to serialize a non-nullable
            // string field that is set to null. Implementations of
            // System.Xml.XmlWriter may successfully serialize it, so we need
            // to check and throw explicitly before that.
            if (value == null)
            {
                throw new NullReferenceException(
                   "Attempted to serialize a null string. This may indicate a non-nullable string field that was set to null.");
            }
            writer.WriteValue(value);
        }
        #endregion
        #endregion

        void PushNamespace(Metadata metadata)
        {
            if (ns == null) return;

            var prefixLength = ns.Count + 1;
            var prefix = prefixLength > metadata.name.Length ?
                metadata.name.PadRight(prefixLength, '_') :
                metadata.name.Substring(0, prefixLength);

            prefix = prefix.EncodeXmlName();
            ns.Push(prefix);
            writer.WriteAttributeString("xmlns", prefix, null, metadata.GetXmlNamespace());
        }

        void PopNamespace()
        {
            if (ns != null) ns.Pop();
        }

        string Prefix { get { return ns != null ? ns.Peek() : string.Empty; } }
    }
}
