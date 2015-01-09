// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System.Xml;

    public interface IXmlReader
    {
        void Read();
        void Skip();

        /// <summary>
        /// Indicates the Xml's end has been reached.
        /// </summary>
        bool EOF { get; }

        /// <summary>
        /// Returns the type of the current Xml node in the input.
        /// </summary>
        XmlNodeType NodeType { get; }

        /// <summary>
        /// Returns the local name of the current Xml node in the input.
        /// </summary>
        string LocalName { get; }

        /// <summary>
        /// Returns the Xml namesapce URI of the current Xml node in the input.
        /// </summary>
        string NamespaceURI { get; }

        bool IsEmptyElement { get; }

        string Value { get; }
    }
}
