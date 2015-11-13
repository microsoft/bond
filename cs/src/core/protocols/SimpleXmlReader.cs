// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System.IO;
    using System.Runtime.CompilerServices;
    using System.Xml;

    using Bond.Expressions.Xml;

    [Parser(typeof(SimpleXmlParser<>))]
    public struct SimpleXmlReader : IXmlReader
    {
        readonly XmlReader reader;

        public SimpleXmlReader(XmlReader reader)
            : this()
        {
            this.reader = reader;
        }

        public SimpleXmlReader(Stream stream)
            : this(XmlReader.Create(stream, new XmlReaderSettings 
                {
                    IgnoreComments = true,
                    IgnoreProcessingInstructions = true
                }))
        { }

        public bool EOF
        {
            get { return reader.EOF; }
        }

        public XmlNodeType NodeType
        {
            get { return reader.NodeType; }
        }

        public string LocalName
        {
            get { return reader.LocalName; }
        }

        public string NamespaceURI
        {
            get { return reader.NamespaceURI; }
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Read()
        {
            reader.Read();
        }

#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public void Skip()
        {
            reader.Skip();
        }

        public bool IsEmptyElement
        {
            get { return reader.IsEmptyElement; }
        }

        public string Value
        {
            get { return reader.Value; }
        }
    }
}
