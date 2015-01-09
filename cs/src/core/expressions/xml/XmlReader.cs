// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Xml
{
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Protocols;

    public class XmlReader<R> where R : IXmlReader
    {
        private static readonly MethodInfo read = Reflection.MethodInfoOf((IXmlReader r) => r.Read());
        private static readonly MethodInfo skip = Reflection.MethodInfoOf((IXmlReader r) => r.Skip());
        private static readonly PropertyInfo eof = Reflection.PropertyInfoOf((IXmlReader r) => r.EOF);
        private static readonly PropertyInfo nodeType = Reflection.PropertyInfoOf((IXmlReader r) => r.NodeType);
        private static readonly PropertyInfo localName = Reflection.PropertyInfoOf((IXmlReader r) => r.LocalName);
        private static readonly PropertyInfo namespaceUri = Reflection.PropertyInfoOf((IXmlReader r) => r.NamespaceURI);
        private static readonly PropertyInfo isEmptyElement = Reflection.PropertyInfoOf((IXmlReader r) => r.IsEmptyElement);
        private static readonly PropertyInfo value = Reflection.PropertyInfoOf((IXmlReader r) => r.Value);

        private readonly ParameterExpression reader = Expression.Parameter(typeof(R), "reader");

        public ParameterExpression Param { get { return reader; } }

        public Expression EOF { get { return Expression.Property(reader, eof); } }

        public Expression NodeType { get { return Expression.Property(reader, nodeType); } }

        public Expression LocalName { get { return Expression.Property(reader, localName); } }

        public Expression NamespaceURI { get { return Expression.Property(reader, namespaceUri); } }
        
        public Expression Read() { return Expression.Call(reader, read); }

        public Expression Skip() { return Expression.Call(reader, skip); }

        public Expression IsEmptyElement { get { return Expression.Property(reader, isEmptyElement); } }

        public Expression Value { get { return Expression.Property(reader, value); } }
    }
}
