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

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteStructBegin(Metadata metadata)
        {
            writer.WriteStartElement(metadata.GetXmlName());
            PushNamespace(metadata);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBaseBegin(Metadata metadata)
        {
            PushNamespace(metadata);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteStructEnd()
        {
            PopNamespace();
            writer.WriteEndElement();
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBaseEnd()
        {
            PopNamespace();
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldBegin(BondDataType dataType, ushort id, Metadata metadata)
        {
            writer.WriteStartElement(Prefix, metadata.name, null);
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldEnd()
        {
            writer.WriteEndElement();
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteFieldOmitted(BondDataType dataType, ushort id, Metadata metadata)
        { }

        #endregion

        #region Containers

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteContainerBegin(int count, BondDataType elementType)
        { }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
        { }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteContainerEnd()
        { }

        #region ITextProtocolWriter

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif     
        public void WriteItemBegin()
        {
            writer.WriteStartElement("Item");
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif        
        public void WriteItemEnd()
        {
            writer.WriteEndElement();
        }

        #endregion

        #endregion

        #region Scalars

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
        public void WriteUInt8(byte value)
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
            writer.WriteValue(value.ToString());
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

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void WriteBool(bool value)
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
        public void WriteWString(string value)
        {
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
